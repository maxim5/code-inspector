/* stream.cc
 *   by Trinity Quirk <tquirk@ymb.net>
 *   last updated 09 Jul 2014, 13:38:40 trinityquirk
 *
 * Revision IX game server
 * Copyright (C) 2014  Trinity Annabelle Quirk
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 *
 * This file contains the implementation of the stream server socket.
 *
 * Changes
 *   09 Sep 2007 TAQ - Created the file from the ashes of tcpserver.c.
 *   13 Sep 2007 TAQ - Removed server.h include.  Used basesock's static
 *                     create_socket instead of C version.
 *   23 Sep 2007 TAQ - The constructor of ThreadPool changed.
 *   16 Dec 2007 TAQ - Added timestamp and pending_logout members to
 *                     assignment operator for stream_user.  Added thread
 *                     routine to reap link-dead and logged-out users.
 *   22 Nov 2009 TAQ - Fixed typo (subserv != subsrv) in stream_reaper_worker.
 *                     Redeclared stream_reaper_worker as extern, so it
 *                     can be a friend to stream_socket.
 *   19 Sep 2013 TAQ - Return NULL at the end of the worker routine to quiet
 *                     gcc.
 *   11 May 2014 TAQ - We've moved the motion- and position-related parameters
 *                     out of the GameObject and into the Motion object, so
 *                     some pointers point at different things.
 *   14 Jun 2014 TAQ - Restructured the base classes and the relationship of
 *                     the derived types to the actual socket.
 *   15 Jun 2014 TAQ - Moved the send worker into the class as well.
 *   21 Jun 2014 TAQ - Converted syslog to use the logger stream.
 *   01 Jul 2014 TAQ - Base class got an access pool, and virtuals changed
 *                     a little bit.
 *   04 Jul 2014 TAQ - Instead of moving subserver here, we separated it out
 *                     into a completely separate binary and exec it once we
 *                     get the file descriptors set up correctly.
 *   05 Jul 2014 TAQ - This file didn't really need the zone_interface.
 *   09 Jul 2014 TAQ - Normalized the exception-throwing.
 *
 * Things to do
 *
 */

#include <unistd.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <errno.h>
#ifdef __APPLE__
#include <signal.h>
#endif

#include <algorithm>
#include <sstream>
#include <stdexcept>

#include "stream.h"
#include "game_obj.h"

#include "../config.h"
#include "../log.h"

extern volatile int main_loop_exit_flag;

stream_user::stream_user(u_int64_t u, Control *c)
    : base_user(u, c)
{
    this->subsrv = 0;
    this->fd = 0;
}

const stream_user& stream_user::operator=(const stream_user& su)
{
    this->subsrv = su.subsrv;
    this->fd = su.fd;
    this->base_user::operator=(su);
    return *this;
}

const stream_socket::subserver& stream_socket::subserver::operator=(const stream_socket::subserver& ss)
{
    this->sock = ss.sock;
    this->pid = ss.pid;
    this->connections = ss.connections;
    return *this;
}

stream_socket::stream_socket(struct addrinfo *ai, u_int16_t port)
    : listen_socket(ai, port), subservers()
{
    FD_ZERO(&(this->master_readfs));
    FD_SET(this->sock.sock, &(this->master_readfs));
    this->max_fd = this->sock.sock + 1;
}

int stream_socket::create_subserver(void)
{
    int i, fd[2], pid, retval = -1;

    if (socketpair(AF_UNIX, SOCK_STREAM, 0, fd) == -1)
    {
        std::clog << syslogErr
                  << "couldn't create subserver sockets for stream port "
                  << this->sock.port_num << ": "
                  << strerror(errno) << " (" << errno << ")" << std::endl;
        return retval;
    }

    /* We want the port number available to the child, for error reporting */
    i = this->sock.port_num;

    if ((pid = fork()) < 0)
    {
        std::clog << syslogErr
                  << "couldn't fork new subserver for stream port "
                  << this->sock.port_num << ": "
                  << strerror(errno) << " (" << errno << ")" << std::endl;
        close(fd[0]);
        close(fd[1]);
    }
    else if (pid == 0)
    {
        /* We are the child; close all sockets except for the one
         * which connects us to the main server.  We call closelog
         * explicitly because it doesn't seem to work otherwise.
         */
        struct rlimit rlim;

        if (getrlimit(RLIMIT_NOFILE, &rlim) != 0)
        {
            std::clog << syslogErr
                      << "can't get rlimit in child " << getpid()
                      << " for stream port " << i << ": "
                      << strerror(errno) << " (" << errno << ")" << std::endl;
            std::clog << syslogErr << "terminating child " << getpid()
                      << " for stream port " << i << std::endl;
            _exit(1);
        }

        /* Close all the open files except for our server
         * communication socket.
         */
        closelog();
        dup2(fd[1], STDIN_FILENO);
        for (i = 0; i < (int)rlim.rlim_cur; ++i)
            if (i != STDIN_FILENO)
                close(i);

        /* This call will never return */
        execlp(SUBSERVER_PROG, NULL);

        /* And in case it does... */
        close(STDIN_FILENO);
        _exit(0);
    }
    else
    {
        /* We are the parent */
        stream_socket::subserver new_sub;

        close(fd[1]);
        new_sub.sock = fd[0];
        new_sub.pid = pid;
        new_sub.connections = 0;
        this->max_fd = std::max(new_sub.sock + 1, this->max_fd);
        this->subservers.push_back(new_sub);
        retval = this->subservers.size() - 1;
        std::clog << "created subserver " << retval
                  << " for stream port " << this->sock.port_num
                  << ", sock " << new_sub.sock
                  << ", pid " << new_sub.pid << std::endl;
    }
    return retval;
}

/* The next function is the load-balancing functions which is used
 * to pick the subserver to which we'll hand off a child socket.
 *
 * The load balancing routine tries to keep a balance between all
 * subservers, and spawns a new subserver when the load on all the
 * current ones exceeds the threshold value.
 *
 * Logic dictates that smaller values of load_threshold would work
 * better with larger values of min_subservers.  Of course, in
 * practice it might be very different.
 *
 * It looks like we're going to have to deny connections at some
 * point, if the load gets way too great.  Our return value for that
 * will be -1.
 */
int stream_socket::choose_subserver(void)
{
    int i, lowest_load = config.max_subservers, best_index = -1;

    for (i = 0; i < (int)this->subservers.size(); ++i)
        if (this->subservers[i].connections < (int)(config.max_subservers
                                                    * config.load_threshold)
            && this->subservers[i].connections < lowest_load)
        {
            lowest_load = this->subservers[i].connections;
            best_index = i;
        }
    if (best_index == -1
        && (int)this->subservers.size() < config.max_subservers)
        best_index = this->create_subserver();
    return best_index;
}

/* This next function is ripped directly out of the W. Richard Stevens
 * "Advanced Programming in the UNIX Environment" book, pages 487-488.
 * Some names were changed to protect the innocent.
 */
int stream_socket::pass_fd(int fd, int new_fd)
{
    struct iovec iov[1];
    struct msghdr msg;
    char buf[2];

    /* We will use the BSD4.4 method, since Linux uses that method */
    iov[0].iov_base = buf;
    iov[0].iov_len = 2;
    msg.msg_iov = iov;
    msg.msg_iovlen = 1;
    msg.msg_name = NULL;
    msg.msg_namelen = 0;
    if (new_fd < 0)
    {
        msg.msg_control = NULL;
        msg.msg_controllen = 0;
        buf[1] = -fd;
        if (buf[1] == 0)
            buf[1] = 1;
    }
    else
    {
        this->cmptr.cmsg_level = SOL_SOCKET;
        this->cmptr.cmsg_type = SCM_RIGHTS;
        this->cmptr.cmsg_len = sizeof(struct cmsghdr) + sizeof(int);
        msg.msg_control = (caddr_t)(&this->cmptr);
        msg.msg_controllen = sizeof(struct cmsghdr) + sizeof(int);
        *(int *)CMSG_DATA((&this->cmptr)) = new_fd;
        buf[1] = 0;
    }
    buf[0] = 0;
    if (sendmsg(fd, &msg, 0) != 2)
        return -1;
    return 0;
}

stream_socket::~stream_socket()
{
    std::vector<subserver>::iterator i;

    if (this->sock.sock != 0)
        FD_CLR(this->sock.sock, &this->master_readfs);

    /* Kill each of the subservers, and empty the vector */
    for (i = this->subservers.begin(); i != this->subservers.end(); ++i)
    {
        if (kill((*i).pid, SIGTERM) == -1)
            std::clog << syslogErr
                      << "couldn't kill subserver " << (*i).pid
                      << " for stream port " << this->sock.port_num << ": "
                      << strerror(errno) << " (" << errno << ")" << std::endl;
        waitpid((*i).pid, NULL, 0);
        FD_CLR((*i).sock, &this->master_readfs);
        close((*i).sock);
    }
    this->subservers.erase(this->subservers.begin(), this->subservers.end());

    /* Thread pools are handled by the listen_socket destructor */
}

void stream_socket::start(void)
{
    int i, retval;

    std::clog << "starting connection loop for stream port "
              << this->sock.port_num << std::endl;

    /* Before we go into the select loop, we should spawn off the
     * number of subservers in min_subservers.
     */
    for (i = 0; i < config.min_subservers; ++i)
        if ((this->create_subserver()) == -1)
        {
            std::ostringstream s;
            s << "couldn't create subserver for stream port "
              << this->sock.port_num << ": "
              << strerror(errno) << " (" << errno << ")";
            throw std::runtime_error(s.str());
        }

    /* Start up the sending thread pool */
    sleep(0);
    this->send_pool->startup_arg = (void *)this;
    this->send_pool->start(stream_socket::stream_send_worker);
    this->access_pool->startup_arg = (void *)this;
    this->access_pool->start(listen_socket::access_pool_worker);
    this->sock.listen_arg = (void *)this;
    this->sock.start(stream_socket::stream_listen_worker);

    /* Start up the reaping thread */
    if ((retval = pthread_create(&this->reaper, NULL,
                                 stream_reaper_worker, (void *)this)) != 0)
    {
        std::ostringstream s;
        s << "couldn't create reaper thread for stream port "
          << this->sock.port_num << ": "
          << strerror(retval) << " (" << retval << ")";
        throw std::runtime_error(s.str());
    }
}

void stream_socket::do_login(u_int64_t userid, Control *con, access_list& al)
{
    stream_user *stu = new stream_user(userid, con);
    stu->subsrv = al.what.login.who.stream.sub;
    stu->fd = al.what.login.who.stream.sock;
    users[userid] = stu;
}

void *stream_socket::stream_listen_worker(void *arg)
{
    stream_socket *sts = (stream_socket *)arg;
    int fd, retval, len;
    unsigned char buf[1024];
    struct sockaddr_in sin;
    socklen_t slen;
    struct linger ls;
    std::vector<subserver>::iterator i, j;

    for (;;)
    {
        /* The readfs state is undefined after select; define it. */
        memcpy(&(sts->readfs), &(sts->master_readfs), sizeof(fd_set));

        pthread_testcancel();
        if ((retval = select(sts->max_fd, &(sts->readfs),
                             NULL, NULL, NULL)) == 0)
            continue;
        else if (retval == -1)
        {
            pthread_testcancel();
            if (errno == EINTR)
            {
                /* we got a signal; it could have been a HUP */
                std::clog << syslogNotice
                          << "select interrupted by signal in stream port "
                          << sts->sock.port_num << std::endl;
                continue;
            }
            else
            {
                std::clog << syslogErr
                          << "select error in stream port "
                          << sts->sock.port_num << ": "
                          << strerror(errno) << " (" << errno << ")"
                          << std::endl;
                /* Should we blow up or something here? */
            }
        }

        pthread_testcancel();
        /* We got some data from somebody. */
        /* Figure out if it's a listening socket. */
        if (FD_ISSET(sts->sock.sock, &(sts->readfs))
            && (fd = accept(sts->sock.sock,
                            (struct sockaddr *)&sin, &slen)) > 0)
        {
            /* Pass the new fd to a subserver immediately.
             * First, set some good socket options, based on
             * our config options.
             */
            ls.l_onoff = (config.use_linger > 0);
            ls.l_linger = config.use_linger;
            setsockopt(fd, SOL_SOCKET, SO_LINGER,
                       &ls, sizeof(struct linger));
            setsockopt(fd, SOL_SOCKET, SO_KEEPALIVE,
                       &config.use_keepalive, sizeof(int));
            ioctl(fd, FIONBIO, &config.use_nonblock);
            if ((retval = sts->choose_subserver()) != -1)
            {
                /* We have connections available */
                sts->pass_fd(sts->subservers[retval].sock, fd);
                ++(sts->subservers[retval].connections);
            }
            else
                /* We should send some kind of "maximum number of
                 * connections exceeded" message here.
                 */
                close(fd);
        }

        for (i = sts->subservers.begin(); i != sts->subservers.end(); ++i)
            if (FD_ISSET((*i).sock, &(sts->readfs)))
            {
                /* It's a subserver socket. */
                if ((len = read((*i).sock,
                                buf,
                                sizeof(buf))) > 0)
                {
                    /* We have recieved something. */
                    std::clog << "got something" << std::endl;

                    /* The first sizeof(int) bytes will be the socket
                     * descriptor, and the rest of it will be the actual
                     * client data that we got.
                     */
                }
                else
                {
                    /* This subserver has died. */
                    std::clog << "stream port " << sts->sock.port_num
                              << " subserver " << (*i).pid
                              << " died" << std::endl;
                    waitpid((*i).pid, NULL, 0);
                    close((*i).sock);
                    FD_CLR((*i).sock, &(sts->master_readfs));
                    sts->subservers.erase(i--);

                    /* Make sure max_fd is valid. */
                    sts->max_fd = sts->sock.sock + 1;
                    for (j = sts->subservers.begin();
                         j != sts->subservers.end();
                         ++j)
                        sts->max_fd = std::max((*j).sock + 1, sts->max_fd);
                }
            }
    }
    std::clog << "exiting connection loop for stream port "
              << sts->sock.port_num << std::endl;
    return NULL;
}

void *stream_socket::stream_reaper_worker(void *arg)
{
    stream_socket *sts = (stream_socket *)arg;
    std::map<u_int64_t, base_user *>::iterator i;
    stream_user *stu;
    time_t now;

    std::clog << "started reaper thread for stream port "
              << sts->sock.port_num << std::endl;
    for (;;)
    {
        sleep(listen_socket::REAP_TIMEOUT);
        now = time(NULL);
        for (i = sts->users.begin(); i != sts->users.end(); ++i)
        {
            pthread_testcancel();
            stu = dynamic_cast<stream_user *>((*i).second);
            if (stu->timestamp < now - listen_socket::LINK_DEAD_TIMEOUT)
            {
                /* We'll consider the user link-dead */
                std::clog << "removing user "
                          << stu->control->username
                          << " (" << stu->userid << ") from stream port "
                          << sts->sock.port_num << std::endl;
                if (stu->control->slave != NULL)
                {
                    /* Clean up a user who has logged out */
                    stu->control->slave->object->natures["invisible"] = 1;
                    stu->control->slave->object->natures["non-interactive"] = 1;
                }
                delete stu->control;
                /* Tell subserver stu->subserv to close and erase user
                 * stu->fd
                 * To do this, we'll simply pass the descriptor again.
                 */
                sts->pass_fd(stu->subsrv, stu->fd);
                sts->users.erase((*(i--)).second->userid);
            }
            else if (stu->timestamp < now - listen_socket::PING_TIMEOUT
                     && stu->pending_logout == false)
                /* After 30 seconds, see if the user is still there */
                stu->control->send_ping();
        }
        pthread_testcancel();
    }
    return NULL;
}

void *stream_socket::stream_send_worker(void *arg)
{
    stream_socket *sts = (stream_socket *)arg;
    stream_user *stu;
    packet_list req;
    size_t realsize;

    std::clog << "started send pool worker for stream port "
              << sts->sock.port_num << std::endl;
    for (;;)
    {
        sts->send_pool->pop(&req);

        realsize = packet_size(&req.buf);
        if (hton_packet(&req.buf, realsize))
        {
            stu = dynamic_cast<stream_user *>(sts->users[req.who]);
            if (stu == NULL)
                continue;
            /* TODO: Encryption */
            stu->control->send(&(req.buf));
        }
    }
    std::clog << "exiting send pool worker for stream port "
              << sts->sock.port_num << std::endl;
    return NULL;
}
