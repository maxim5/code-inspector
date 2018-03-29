#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <signal.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>
#include <pthread.h>

/* network, sockets, accept, IP handling */
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>

#include <fcntl.h>
#include <time.h>

// #define HAVE_LUA

#ifdef HAVE_LUA
#include "lua.h"
#include "lualib.h"
#include "lauxlib.h"
#endif

/* A few constants */
#define BACK_LOG                 5

#define MAX_REQUEST_LENGTH       256
#define MAX_READ_LENGTH          94
#define BLOCK_SIZE               4096
#define RECV_BUFF_LENGTH         8196

#define INITIAL_CONNS            5
/* experimental number */
#define WORKER_THREADS           2
#define HTTP_PORT                8001
#define HTTP_VERSION             "HTTP/1.1"
#define DEBUG_VERBOSE            2

/* ########################### DATA STRUCTURES ############################# */
enum bool
{
	false,
	true
};

enum req_states
{
	REQSTATE_READ_HEAD,
	REQSTATE_SEND_HEAD,
	REQSTATE_BUFF_HEAD,
	REQSTATE_SEND_FILE
};

/* contain all metadata regarding one connection */
struct cn_strct
{
	/* doubly linked list for the _Busy_conns */
	struct  cn_strct     *c_next;
	struct  cn_strct     *c_prev;
	/* single linked list for queue FIFO styled */
	struct  cn_strct     *q_prev;
	/* basic information */
	enum    req_states    req_state;
	int                   net_socket;
	/* incoming buffer */
	char                 *data_buf_head;    /* points to start, always */
	char                 *data_buf;         /* points to current spot */
	int                   processed_bytes;
	/* head information */
	char                 *url;

	enum    bool          is_static;
	int                   identifier;       /* DEBUG: keep track of structs */
};

/* ######################## FUNCTION DECLARATIONS ########################## */
/* Forward declaration of some connection helpers */
static int   create_listener        ( int port );
static void  handle_new_conn        ( int listenfd );
static void  add_conn_to_list       ( int sd, char *ip );
static void  remove_conn_from_list  ( struct cn_strct *cn );

/* Forward declaration of select's processing helpers */
static void  read_request           ( struct cn_strct *cn );
static void  write_head             ( struct cn_strct *cn );
static void  send_file              ( struct cn_strct *cn );
/* debug */
#if DEBUG_VERBOSE == 2
static void  list_list              ( struct cn_strct *nd );
static void  list_queue             ( struct cn_strct *nd, int count );
#endif
static void  show_cn                ( struct cn_strct *cn);

/* Forward declaration of app bound methods */
static void *run_app_thread         ( void *tid );

#ifdef HAVE_LUA
static int   l_buffer_output        ( lua_State *L );

/* set up the Lua bindings for C-functions */
static const struct luaL_reg app_lib [] = {
	{"prepare",  l_buffer_output},
	{NULL,       NULL}
};
#else
static void  c_response             ( struct cn_strct *cn);
#endif

/* ######################## GLOBAL VARIABLES ############################### */
struct cn_strct     *_Free_conns;       /* idleing conns, LIFO stack */
int                  _Free_count;
struct cn_strct     *_Busy_conns;       /* working conns, doubly linked list */
int                  _Busy_count;
const char * const   _Server_version = "testserver/poc";
int                  _Master_sock;      /* listening master socket */
time_t               _Last_loop;        /* marks the last run of select */
char                 _Master_date[30];  /* the formatted date */
int                  _Conn_count;       /* all existing cn_structs */

/* a FIFO stack for quead up conns waiting for threads */
struct cn_strct     *_Queue_head;
struct cn_strct     *_Queue_tail;
int                  _Queue_count;
pthread_mutex_t wake_worker_mutex  = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t pull_job_mutex     = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t  wake_worker_cond   = PTHREAD_COND_INITIALIZER;
pthread_t       _Workers[WORKER_THREADS]; /* used to clean up */

/* ####################### STARTING THE ACTUAL IMPLEMENTATION ############## */
/* clean up after ourselves */
static void
clean_on_quit(int sig)
{
	struct cn_strct *tp;
	int i;
#if DEBUG_VERBOSE == 2
	printf("\n\n\n\n\nPRINTING QUEUE: \n");
	list_queue(_Queue_head, _Queue_count);
	printf("PRINTING QUEUE_LIST: \n");
	list_list(_Queue_tail);
	printf("PRINTING FREEs: \n");
	list_list(_Free_conns);
	printf("PRINTING BUSYs: \n");
	list_list(_Busy_conns);
#endif

	while (NULL != _Free_conns) {
		tp = _Free_conns->c_next;
		free(_Free_conns->data_buf_head);
		free(_Free_conns);
		_Free_conns = tp;
	}

	while (NULL != _Busy_conns) {
		tp = _Busy_conns->c_next;
		show_cn(_Busy_conns);
		free(_Busy_conns->data_buf_head);
		close(_Busy_conns->net_socket);
		free(_Busy_conns);
		_Busy_conns = tp;
	}
	close(_Master_sock);
	_Master_sock = -1;
	printf("Graceful exit done after signal: %d\n", sig);

	/* cleanup the threads */
	for (i = 0; i < WORKER_THREADS; i++) {
		pthread_cancel(_Workers[i]);
	}

	exit(0);
}

static void
die(int sig)
{
	printf("Server stopped, caught signal: %d\n", sig);
	exit(0);
}

/* Run through the connections find, who was a bad boy! */
static void
check_sockets (int sig)
{
	struct cn_strct *tmp1, *tmp2;
	tmp1 = _Busy_conns;
	while (NULL != tmp1) {
		tmp2 = tmp1->c_next;
		//show_cn(tmp1);
		tmp1 = tmp2;
	}
}


int
main(int argc, char *argv[])
{
	fd_set              rfds, wfds;
	struct cn_strct    *tp, *to;
	int                 rnum, wnum, readsocks;
	int                 i;
	struct    tm       *tm_struct;

	/* initialize the masterdate we update only every second */
	_Last_loop = time(NULL);
	tm_struct  = gmtime(&_Last_loop);
	strftime( _Master_date, 32, "%a, %d %b %Y %H:%M:%S %Z", tm_struct);
#if DEBUG_VERBOSE == 2
	_Conn_count=0;
#endif
#if DEBUG_VERBOSE == 1
	printf("STARTED AT: %s\n", _Master_date);
#endif

	signal(SIGQUIT, die);
	signal(SIGTERM, die);
	signal(SIGPIPE, check_sockets);
	signal(SIGINT,  clean_on_quit);

	/* Fill up the initial connection lists; _Free_conns is just a LIFO stack,
	 * there shall never be a performance issues -> single linked only */
	_Free_count=0;
	for (i = 0; i < INITIAL_CONNS; i++) {
		tp = _Free_conns;
		_Free_conns = (struct cn_strct *) calloc(1, sizeof(struct cn_strct));
		_Free_conns->data_buf_head =
			(char *) calloc (RECV_BUFF_LENGTH, sizeof (char));
		_Free_conns->c_next = tp;
		_Free_conns->c_prev = NULL;
		_Free_conns->q_prev = NULL;
		_Free_conns->identifier = _Conn_count++;
		_Free_count++;
	}

	/* create the master listener */
	if ((_Master_sock = create_listener(HTTP_PORT)) == -1) {
		fprintf(stderr, "ERR: Couldn't bind to port %d\n",
				HTTP_PORT);
		exit(1);
	}

	/* set up LIFO queue */
	_Queue_tail = _Queue_head = NULL;
	_Queue_count = 0;

	/* create workers for application */
	for(i = 0; i < WORKER_THREADS; i++) {
		pthread_create(&_Workers[i], NULL, &run_app_thread, (void *) &i);
	}
	sleep(1);
	for(i = 0; i < WORKER_THREADS; i++) {
		pthread_detach( _Workers[i] );
	}

#if DEBUG_VERBOSE == 1
	printf("%s: listening on port %d (http)\n",
			_Server_version, HTTP_PORT);
#endif

	/* main loop */
	while (1) {
		// clean socket lists
		FD_ZERO(&rfds);
		FD_ZERO(&wfds);
		wnum = -1;

		// Add master listener to reading sockets
		FD_SET(_Master_sock, &rfds);
		rnum = _Master_sock;

		// Add the established sockets
		tp = _Busy_conns;

		/* Adding connection to the SocketSets based on state */
		while (tp != NULL) {
			if (REQSTATE_READ_HEAD == tp->req_state) {
				FD_SET(tp->net_socket, &rfds);
				rnum = (tp->net_socket > rnum) ? tp->net_socket : rnum;
			}
			if (REQSTATE_SEND_HEAD == tp->req_state) {
				FD_SET(tp->net_socket, &wfds);
				wnum = (tp->net_socket > wnum) ? tp->net_socket : wnum;
			}
			if (REQSTATE_BUFF_HEAD == tp->req_state) {
				FD_SET(tp->net_socket, &wfds);
				wnum = (tp->net_socket > wnum) ? tp->net_socket : wnum;
			}
			if (REQSTATE_SEND_FILE == tp->req_state) {
				FD_SET(tp->net_socket, &wfds);
				wnum = (tp->net_socket > wnum) ? tp->net_socket : wnum;
			}
			tp = tp->c_next;
		}

		readsocks = select(
			(wnum > rnum) ? wnum+1 : rnum+1,
			(-1 != rnum)  ? &rfds : NULL,
			(-1 != wnum)  ? &wfds : NULL,
			(fd_set *) 0,
			NULL
		);

		// is the main listener in the read set? -> New connection
		if (FD_ISSET(_Master_sock, &rfds)) {
			handle_new_conn(_Master_sock);
			readsocks--;
		}

		// Handle the established sockets
		tp = _Busy_conns;

		while (readsocks > 0 && tp != NULL) {
			to = tp;
			tp = tp->c_next;

			if (REQSTATE_READ_HEAD == to->req_state &&
			  FD_ISSET(to->net_socket, &rfds)) {
				readsocks--;
#if DEBUG_VERBOSE == 1
				printf("WANNA RECV HEAD\n");
#endif
				read_request(to);
			}
			if (REQSTATE_SEND_HEAD == to->req_state &&
			  FD_ISSET(to->net_socket, &wfds)) {
				readsocks--;
#if DEBUG_VERBOSE == 1
				printf("WANNA SEND HEAD\n");
#endif
				write_head(to);
			}
			if (REQSTATE_SEND_FILE == to->req_state &&
			  FD_ISSET(to->net_socket, &wfds)) {
				readsocks--;
#if DEBUG_VERBOSE == 1
				printf("WANNA SEND FILE\n");
#endif
				send_file(to);
			}
		}
	}
	return 0;
}


/*____ ___  _   _ _   _   _   _ _____ _     ____  _____ ____  ____
 / ___/ _ \| \ | | \ | | | | | | ____| |   |  _ \| ____|  _ \/ ___|
| |  | | | |  \| |  \| | | |_| |  _| | |   | |_) |  _| | |_) \___ \
| |__| |_| | |\  | |\  | |  _  | |___| |___|  __/| |___|  _ < ___) |
 \____\___/|_| \_|_| \_| |_| |_|_____|_____|_|   |_____|_| \_\____/ */
/* create the master listening socket */
static int
create_listener(int port)
{
	int                 tmp_s=0, sd;
	struct sockaddr_in  my_addr;

	if ((sd = socket(AF_INET, SOCK_STREAM, 0)) == -1)
		return -1;

	memset(&my_addr, 0, sizeof(my_addr));
	my_addr.sin_family      = AF_INET;
	my_addr.sin_port        = htons((short)port);
	my_addr.sin_addr.s_addr = INADDR_ANY;

	if (0 > setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, &tmp_s, sizeof(int)) ) {
		printf("Failed to reuse the listener socket\n");
		close(sd);
		return -1;
	}
	if (bind(sd, (struct sockaddr *)&my_addr, sizeof(struct sockaddr)) == -1) {
		close(sd);
		return -1;
	}

	listen(sd, BACK_LOG);
	return sd;
}

/*
 * get a socket and form a cn_strct around it
 *  - either pull it of free_conns or create one
 *  - add it to the tail of _Busy_conns
 * */
static void
add_conn_to_list(int sd, char *ip)
{
	struct cn_strct *tp;

	/* pop a cn_strct from the free list ... or create one */
	if (NULL == _Free_conns) {
		tp = (struct cn_strct *) calloc (1, sizeof(struct cn_strct));
		tp->data_buf_head = (char *) calloc (RECV_BUFF_LENGTH, sizeof (char));
		_Free_count=0;
		tp->identifier = _Conn_count++;
	}
	else {
		tp = _Free_conns;
		_Free_conns = tp->c_next;
		/* TODO: For Later, if we end up reallocing for larger buffers we need
		 * to keep track of how much we need to null over upon reuse
		 */
		memset(tp->data_buf_head, 0, RECV_BUFF_LENGTH * sizeof(char));
		_Free_count--;
	}
	//printf("FREE before done: %d\n", _Free_count);

	tp->data_buf        = tp->data_buf_head;

	/* attach to tail of the _Busy_conns */
	if (NULL == _Busy_conns) {
		//printf("ATTACH TO EMPTY BUSY CONNS at %d\n", _Busy_count);
		tp->c_next          = NULL;
		tp->c_prev          = NULL;
		_Busy_conns         = tp;
	}
	else {
		//printf("ATTACH TO BUSY CONNS TAIL at %d\n", _Busy_count);
		tp->c_next          = _Busy_conns;
		_Busy_conns->c_prev = tp;
		_Busy_conns         = tp;
	}
	_Busy_count++;
	//_Busy_conns->c_prev  = NULL;
	tp->net_socket = sd;
	/* make sure the FIFO queue pointer is empty */
	tp->q_prev     = NULL;

	/* Pre/Re-set initial variables */
	tp->processed_bytes  = 0;
	tp->is_static        = false;
	tp->req_state        = REQSTATE_READ_HEAD;
}

static void
handle_new_conn( int listen_sd )
{
	int x;
	struct sockaddr_in their_addr;
	socklen_t tp = sizeof(struct sockaddr_in);
	int connfd = accept(listen_sd, (struct sockaddr *)&their_addr, &tp);
	x = fcntl(connfd, F_GETFL, 0);              /* Get socket flags */
	fcntl(connfd, F_SETFL, x | O_NONBLOCK);     /* Add non-blocking flag */
	add_conn_to_list(connfd, inet_ntoa(their_addr.sin_addr));
}

void
remove_conn_from_list( struct cn_strct *cn )
{
	struct cn_strct *tp;

	tp = cn;

	if (tp == NULL || cn == NULL)
		return;

	if (NULL == tp->c_prev) {          /* tail of _Busy_conns */
		//printf("REMOVE BUSY TAIL at %d\n", _Busy_count);
		if (NULL == tp->c_next) {      /* only one in the list */
			//printf("BUSY TAIL EMPTY\n");
			_Busy_conns = NULL;
		}
		else {
			tp->c_next->c_prev  = NULL;
			_Busy_conns         = tp->c_next;
		}
		_Busy_count--;
	}
	else if (NULL == tp->c_next) {    /* head of _Busy_conns */
		//printf("REMOVE FROM BUSY HEAD at %d\n", _Busy_count);
		tp->c_prev->c_next  = NULL;
		tp->c_prev          = NULL;
		_Busy_count--;
	}
	else {
		//printf("REMOVE FROM INNER BUSY at %d\n", _Busy_count);
		tp->c_prev->c_next = tp->c_next;
		tp->c_next->c_prev = tp->c_prev;
	}

	/* Attach to the end of the _Free_conns, only single link it with c_next */
	cn->c_next          = _Free_conns;
	cn->c_prev          = NULL;
	_Free_conns         = cn;
	_Free_count++;
	// printf("FREE after done: %d\n", _Free_count);

	/* Close it all down */
	if (cn->net_socket != -1) {
		close(cn->net_socket);
	}
}


/*___  _____ _     _____ ____ _____   ____  ____   ___   ____
/ ___|| ____| |   | ____/ ___|_   _| |  _ \|  _ \ / _ \ / ___|
\___ \|  _| | |   |  _|| |     | |   | |_) | |_) | | | | |
 ___) | |___| |___| |__| |___  | |   |  __/|  _ <| |_| | |___
|____/|_____|_____|_____\____| |_|   |_|   |_| \_\\___/ \____| */
/* Here is the deal, we read as much as we can into the empty buffer, then
 * reset the buffer pointer to the end of the read material and append at
 * next read
 */
void
read_request( struct cn_strct *cn )
{
	char *next;
	int   num_recv;

	/* FIXME: For now assume that RECV_BUFF_LENGTH is enough for one read */
	num_recv = recv(
		cn->net_socket,
		cn->data_buf,
		RECV_BUFF_LENGTH - cn->processed_bytes,
		//MAX_READ_LENGTH,
		0
	);

	// sanity check
	if (num_recv <= 0) {
		remove_conn_from_list(cn);
		return;
	}

	// set the read pointer to where we left off
	next = cn->data_buf_head + cn->processed_bytes;

	// adjust buffer
	cn->processed_bytes += num_recv;
	cn->data_buf = cn->data_buf_head + cn->processed_bytes;

	/* null terminate the current buffer -> overwrite on next read */
	cn->data_buf_head[cn->processed_bytes] = '\0';

	/* a naive little line parser */
	while ( (*next != '\0') ) {
		if ( *(next)=='\r')
			if (*(next+1)=='\n' &&
			    *(next+2)=='\r' && *(next+3)=='\n' ) {
			// proceed next stage
			cn->req_state = REQSTATE_SEND_HEAD;
		}
		next++;
	}
}

/*
 */
void
write_head (struct cn_strct *cn)
{
	time_t     now = time(NULL);
	struct tm *tm_struct;

	/* prepare the global date string */
	if (now-_Last_loop>0) {
		_Last_loop = now;
		tm_struct = gmtime(&_Last_loop);
		//Sun, 06 Nov 1994 08:49:37 GMT
		strftime( _Master_date, 30, "%a, %d %b %Y %H:%M:%S %Z", tm_struct);
	}

	/* enqueue this connection to the _App_queue */
	pthread_mutex_lock( &pull_job_mutex );
	if (NULL == _Queue_tail) {
		_Queue_tail = _Queue_head = cn;
		_Queue_count = 1;
	}
	else {
		_Queue_tail->q_prev = cn;
		_Queue_tail = cn;
		_Queue_count++;
	}
	pthread_mutex_unlock( &pull_job_mutex );
	cn->req_state = REQSTATE_BUFF_HEAD;

	/* wake a worker to start the application */
	pthread_cond_signal (&wake_worker_cond);
}

void
send_file (struct cn_strct *cn)
{
	int sent = send (cn->net_socket, cn->data_buf,
		cn->processed_bytes, 0);

#if DEBUG_VERBOSE == 3
	printf("%d: sent:%d   ---- left: %d\n", cn->identifier, sent,
		cn->processed_bytes-sent);
#endif
	if (0 > sent) {
		remove_conn_from_list(cn);
	}
	else if (cn->processed_bytes == sent) {
		remove_conn_from_list(cn);
	}
	else if (0 == sent) {
		/* Do nothing */
	}
	else {
		cn->data_buf = cn->data_buf + sent;
		cn->processed_bytes -= sent;
	}
}

/*                                     _
 _ __ _   _ _ __   __      _____  _ __| | _____ _ __
| ' _| | | | '_ \  \ \ /\ / / _ \| '__| |/ / _ \ '__|
| |  | |_| | | | |  \ V  V / (_) | |  |   <  __/ |
|_|   \__,_|_| |_|   \_/\_/ \___/|_|  |_|\_\___|_| */

/*
 * Run the actual application workers, just keep looping through them and see if
 * something is left to do
 */
void
*run_app_thread (void *tid)
{
	struct cn_strct *cn;
	int              id =       *((int*) tid);
	int              sent;

#ifdef HAVE_LUA
	// thread local lua state
	lua_State *L = lua_open();
	luaL_openlibs (L);
	luaL_openlib  (L, "parcle", app_lib, 0);
	luaL_dofile   (L, "../app/_init.lua");
#endif


	while(1) {
		// monitor
		pthread_mutex_lock( &wake_worker_mutex );
		while (NULL == _Queue_head) {
			pthread_cond_wait( &wake_worker_cond, &wake_worker_mutex );
		}
		pthread_mutex_unlock( &wake_worker_mutex );

		/* pull job from queue */
		pthread_mutex_lock   ( &pull_job_mutex );
		if (NULL == _Queue_head) {
			printf("QUEUE MISSED!!\n");
			pthread_mutex_unlock ( &pull_job_mutex );
			continue;
		}
		else {
			cn   =   _Queue_head;
			_Queue_count--;
			if (NULL == _Queue_head->q_prev) {
				_Queue_head = NULL;
				_Queue_tail = NULL;
			}
			else {
				_Queue_head = cn->q_prev;
				cn->q_prev  = NULL;
			}
		}
#if DEBUG_VERBOSE == 3
		if (_Queue_count > 1)
			printf("Left in Queue AFTER REMOVAL: %d\n",
				_Queue_count
			);
#endif
		pthread_mutex_unlock ( &pull_job_mutex );

#ifdef HAVE_LUA
		/* Execute the lua function we want */
		lua_getglobal(L, "test");
		lua_pushlightuserdata(L, (void*) cn);
		lua_call(L, 1, 0);
#else
		c_response(cn);
#endif

		cn->data_buf        = cn->data_buf_head;

		/* Assume we can do it in one rush ... */
		sent = send (cn->net_socket, "", 0, 0);
		cn->req_state       = REQSTATE_SEND_FILE;

		if (REQSTATE_SEND_FILE != cn->req_state) {
			printf("DAMMIT!\n");
			sent = send (cn->net_socket, "", 0, 0);
			cn->req_state       = REQSTATE_SEND_FILE;
		}

		if (REQSTATE_SEND_FILE != cn->req_state)
			printf("FUCK IT!\n");

		/* pick up some slack in case some others missed */
		pthread_cond_signal (&wake_worker_cond);
	}
}


#ifdef HAVE_LUA
/*_                  _ _ _
 | |   _   _  __ _  | (_) |__  ___
 | |  | | | |/ _` | | | | '_ \/ __|
 | |__| |_| | (_| | | | | |_) \__ \
 |_____\__,_|\__,_| |_|_|_.__/|___/

 from here we deal with C functions that will be exposed to Lua as part of the
 par[ck]le library (aka Lua module)*/

/*
 * @param:        the connection pointer
 * @param:        the string reference
 */
static int
l_buffer_output (lua_State *L)
{
	//const char      *data   = NULL;
	struct cn_strct *cn     = NULL;

	cn     =  (struct cn_strct*) lua_touserdata(L, 1);
	strncpy(cn->data_buf_head, lua_tostring(L,2), lua_strlen  (L, 2 ));
	cn->processed_bytes = lua_strlen  (L, 2 );

	return 0;
}
#else
/*
 * A native method that returns a static response into the connections socket
 * It's meant to be used as a test method
 */
static void
c_response ( struct cn_strct *cn)
{
	char *page = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n\
  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\" >\n\
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\" >\n\
<head>\n\
  <title>I'm the Favicon substitute</title>\n\
  <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />\n\
</head>\n\
<body>\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
  <b>I am a line</b>: Amazing isn't it totally blowing your mind! ?! <br />\n\
</body>\n\
</html>\n";
	cn->processed_bytes = snprintf(
		cn->data_buf_head, RECV_BUFF_LENGTH,
		HTTP_VERSION" 200 OK\r\n"
		"Server: %s\r\n"
		"Content-Type: text/html\r\n"
		"Content-Length: %Zd\r\n"
		"Date: %s\r\n"
		"Last-Modified: %s\r\n\r\n%s"
		, _Server_version, strlen(page),
		_Master_date, _Master_date, page
	);
}
#endif

#if DEBUG_VERBOSE == 2
/*
 * a simplistic way to print out linked lists, a very crude visualization but it
 * helps debugging
 */
static void
list_list (struct cn_strct *nd)
{
	struct cn_strct *tmp, *tmp1;
	int              cnt = 0;

	tmp=nd;
	printf( "prev\tdata\tnext\n" );
	while (NULL != tmp) {
		if (tmp == nd && 0 < cnt++) {
			printf("DETECTED LOOP \n");
			break;
		}
		tmp1 = tmp->c_next;
		if (NULL != tmp->c_prev && NULL != tmp->c_next)
			printf("%d\t%d\t%d\n", tmp->c_prev->identifier,
				tmp->identifier, tmp->c_next->identifier );
		else if (NULL == tmp->c_prev && NULL != tmp->c_next)
			printf("  \t%d\t%d\n", tmp->identifier, tmp->c_next->identifier );
		else if (NULL != tmp->c_prev && NULL == tmp->c_next)
			printf("%d\t%d\t  \n", tmp->c_prev->identifier, tmp->identifier);
		else
			printf("  \t%d\t  \n", tmp->identifier);
		tmp=tmp1;
	}
}

static void
list_queue (struct cn_strct *nd, int count)
{
	struct cn_strct *tmp, *tmp1;
	int              cnt = 0;

	tmp=nd;
	printf( "q_prev\tdata\tq_next\n" );
	while (NULL != tmp) {
		if (tmp == nd && 0 < cnt) {
			printf("DETECTED LOOP \n");
			break;
		}
		tmp1 = tmp->q_prev;
		if (NULL == tmp->q_prev)
			printf("  \t%d\t  \t%d\n", tmp->identifier, count);
		else
			printf("%d\t%d\t  \t%d\n", tmp->q_prev->identifier, tmp->identifier, count);
		tmp=tmp1;
		cnt++;
	}
}
#endif


static void
show_cn (struct cn_strct *cn)
{
	if (REQSTATE_BUFF_HEAD == cn->req_state)
		printf("\nIDENTIFIER:  %d\n"
			"REQSTATE:    %d\n"
			"SOCKET:      %d\n"
			"DATA_ALL: %s\n"
			"PROCESSED: %d\n",
			cn->identifier,
			cn->req_state,
			cn->net_socket,
			cn->data_buf_head,
			cn->processed_bytes);
	else
		printf("\nIDENTIFIER:  %d\n"
			"REQSTATE:    %d\n"
			"SOCKET:      %d\n",
			cn->identifier,
			cn->req_state,
			cn->net_socket);
}

// vim: ts=4 sw=4 sts=4 sta tw=80 list
