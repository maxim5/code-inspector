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

#define HAVE_LUA

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
#define HTTP_PORT                8000
#define HTTP_VERSION             "HTTP/1.1"
#define DEBUG_VERBOSE            0
#define WEB_ROOT                 "./"
#define STATIC_ROOT              "webroot"
#define STATIC_ROOT_LENGTH       7
/* special cases served from the STATIC_ROOT directory */
#define FAVICON_URL              "favicon.ico"
#define FAVICON_URL_LENGTH       11
#define ROBOTS_URL               "robots.txt"
#define ROBOTS_URL_LENGTH        10

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
	REQSTATE_BUFF_FILE,
	REQSTATE_SEND_FILE
};

enum req_types
{
	REQTYPE_GET,
	REQTYPE_HEAD,
	REQTYPE_POST
};

enum http_version
{
	HTTP_09,
	HTTP_10,
	HTTP_11
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
	int                   file_desc;
	/* incoming buffer */
	char                 *data_buf_head;    /* points to start, always */
	char                 *data_buf;         /* points to current spot */
	int                   processed_bytes;
	/* inc buffer state */
	int                   line_count;
	/* head information */
	enum    req_types     req_type;
	char                 *url;
	char                 *pay_load;         /* either GET or POST data */
	enum    http_version  http_prot;

	enum    bool          is_static;
#if DEBUG_VERBOSE == 2
	int                   identifier;       /* DEBUG: keep track of structs */
#endif
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
static void  buff_file              ( struct cn_strct *cn );
static void  send_file              ( struct cn_strct *cn );
/* debug */
#if DEBUG_VERBOSE == 2
static void  list_list              ( struct cn_strct *nd );
static void  list_queue             ( struct cn_strct *nd, int count );
static void  show_cn                ( struct cn_strct *cn);
#endif

/* Forward declaration of string parsing methods */
static void  parse_first_line       ( struct cn_strct *cn );
static const char  *getmimetype     ( const char *name );

/* Forward declaration of app bound methods */
static void *run_app_thread         ( void *tid );
static void  c_response             ( struct cn_strct *cn );

#ifdef HAVE_LUA
static int   l_buffer_output        ( lua_State *L );

/* set up the Lua bindings for C-functions */
static const struct luaL_reg app_lib [] = {
	{"commit",   l_buffer_output},
	{NULL,       NULL}
};
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
#if DEBUG_VERBOSE == 2
int                  _Conn_count;       /* all existing cn_structs */
#endif

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
#if DEBUG_VERBOSE == 2
		show_cn(tp);
#endif
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
	signal(SIGINT, clean_on_quit);

	// DIRTY!!! we work out of the webroot directory -> change to it
	if (chdir(WEB_ROOT))
		clean_on_quit(2);

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
#if DEBUG_VERBOSE == 2
		_Free_conns->identifier = _Conn_count++;
#endif
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
			if (REQSTATE_BUFF_FILE == tp->req_state) {
				FD_SET(tp->file_desc, &rfds);
				rnum = (tp->file_desc > rnum) ? tp->file_desc : rnum;
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
			if (REQSTATE_BUFF_FILE == to->req_state &&
			  FD_ISSET(to->file_desc, &rfds)) {
				readsocks--;
#if DEBUG_VERBOSE == 1
				printf("WANNA BUFF FILE\n");
#endif
				buff_file(to);
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
#if DEBUG_VERBOSE == 2
		tp->identifier = _Conn_count++;
#endif
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
	tp->req_state        = REQSTATE_READ_HEAD;
	tp->req_type         = REQTYPE_GET;
	tp->processed_bytes  = 0;
	tp->line_count       = 0;
	tp->pay_load         = '\0';
	tp->is_static        = false;
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
		switch (*next) {
			case '\r':
				if (*(next+1)=='\n' ) {
					cn->line_count++;
					if (1 == cn->line_count) {
						parse_first_line(cn);
					}
					if (*(next+2)=='\r' && *(next+3)=='\n'  ) {
						// proceed next stage
						cn->req_state = REQSTATE_SEND_HEAD;
					}
				}
				break;
			default:
				break;
		}
		next++;
	}
#if DEBUG_VERBOSE == 1
	if (REQSTATE_SEND_HEAD == cn->req_state) {
		printf("METHOD: %d\n",   cn->req_type);
		printf("URL: %s\n",      cn->url);
		printf("PROTOCOL: %d\n", cn->http_prot);
		printf("PAYLOAD: %s\n",  cn->pay_load);
	}
#endif
}

/*
 */
void
write_head (struct cn_strct *cn)
{
	char       buf[RECV_BUFF_LENGTH];
	struct     stat stbuf;
	int        file_exists;
	time_t     now = time(NULL);
	struct tm *tm_struct;

	/* prepare the global date string */
	if (now-_Last_loop>0) {
		_Last_loop = now;
		tm_struct = gmtime(&_Last_loop);
		//Sun, 06 Nov 1994 08:49:37 GMT
		strftime( _Master_date, 30, "%a, %d %b %Y %H:%M:%S %Z", tm_struct);
	}

	/* check if we request a static file */
	if (cn->is_static) {
		cn->url++;              /* eat leading slash */
		if (0 == strncasecmp(cn->url, FAVICON_URL, FAVICON_URL_LENGTH)) {
			file_exists = stat(STATIC_ROOT"/favicon.ico", &stbuf);
		}
		else if (0 == strncasecmp(cn->url, ROBOTS_URL, ROBOTS_URL_LENGTH)) {
			file_exists = stat(STATIC_ROOT"/robots.txt", &stbuf);
		}
		else {
			file_exists = stat(cn->url, &stbuf);
		}

		if (file_exists == -1) {
			//send_error(cn, 404);
			printf("Sorry dude, didn't find the file: %s\n", cn->url);
			remove_conn_from_list(cn);
			return;
		}

		if (0 == strncasecmp(cn->url, FAVICON_URL, FAVICON_URL_LENGTH)) {
			cn->file_desc = open(STATIC_ROOT"/favicon.ico", O_RDONLY);
		}
		else if (0 == strncasecmp(cn->url, ROBOTS_URL, ROBOTS_URL_LENGTH)) {
			cn->file_desc = open(STATIC_ROOT"/robots.txt", O_RDONLY);
		}
		else {
			cn->file_desc = open(cn->url, O_RDONLY);
		}

		snprintf(buf, sizeof(buf),
			HTTP_VERSION" 200 OK\r\n"
			"Server: %s\r\n"
			"Content-Type: %s\r\n"
			"Content-Length: %ld\r\n"
			"Date: %s\r\n\r\n",
			//"Last-Modified: %s\r\n",
			_Server_version,
			getmimetype(cn->url),
			(long) stbuf.st_size,
			_Master_date
			//ctime(&stbuf.st_mtime)
		); /* ctime() has a \n on the end */
		send(cn->net_socket, buf, strlen(buf), 0);

		/* FIXME: we assume the head gets send of in one rush */
		cn->req_state = REQSTATE_BUFF_FILE;
	}
	else {
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
		cn->req_state  = REQSTATE_BUFF_HEAD;

		/* wake a worker to start the application */
		pthread_cond_signal (&wake_worker_cond);   /* we added one -> we wake one */
	}
}


void
buff_file (struct cn_strct *cn)
{
	int rv = read(cn->file_desc, cn->data_buf_head, RECV_BUFF_LENGTH);

#if DEBUG_VERBOSE == 1
	printf("\n\nbuffered:%d\n", rv);
#endif

	cn->data_buf = cn->data_buf_head;

	if (0 >= rv) {
		close(cn->file_desc);
		cn->file_desc = -1;
		remove_conn_from_list(cn);
		return;
	}

	cn->processed_bytes = rv;
	cn->req_state = REQSTATE_SEND_FILE;
}

void
send_file (struct cn_strct *cn)
{
	int rv = send (cn->net_socket, cn->data_buf,
		cn->processed_bytes, 0);

#if DEBUG_VERBOSE == 1
	printf("sent: %d   ---- left: %d\n", rv, cn->processed_bytes-rv);
#endif
	if (0 > rv) {
		remove_conn_from_list(cn);
	}
	else if (cn->processed_bytes == rv) {
		if (cn->is_static)
			cn->req_state = REQSTATE_BUFF_FILE;
		else
			remove_conn_from_list(cn);
	}
	else if (0 == rv) {
		/* Do nothing */
	}
	else {
		cn->data_buf = cn->data_buf + rv;
		cn->processed_bytes -= rv;
	}
}

/*___   _    ____  ____  _____   _   _ _____ _     ____  _____ ____  ____
|  _ \ / \  |  _ \/ ___|| ____| | | | | ____| |   |  _ \| ____|  _ \/ ___|
| |_) / _ \ | |_) \___ \|  _|   | |_| |  _| | |   | |_) |  _| | |_) \___ \
|  __/ ___ \|  _ < ___) | |___  |  _  | |___| |___|  __/| |___|  _ < ___) |
|_| /_/   \_\_| \_\____/|_____| |_| |_|_____|_____|_|   |_____|_| \_\____/ */

/*
 * * Isolate "METHOD URL?GET_PARAMS HTTP_VER" from first request line
 * - count '/' to help the url delimiter, count '?/ to get parser
 */
void
parse_first_line( struct cn_strct *cn )
{
	char          *next  = cn->data_buf_head;
	unsigned short got_get=0, get_cnt=0, slash_cnt=0, error=0;
	/* METHOD */
	if (0 == strncasecmp(next, "GET",  3)) { cn->req_type=REQTYPE_GET;  next+=3;}
	if (0 == strncasecmp(next, "HEAD", 4)) { cn->req_type=REQTYPE_HEAD; next+=4;}
	if (0 == strncasecmp(next, "POST", 4)) { cn->req_type=REQTYPE_POST; next+=4;}
	*next = '\0';
	/* URL */
	next++;
	if ('/' == *next) {
		cn->url = next;
		if (0 == strncasecmp(next+1, STATIC_ROOT, STATIC_ROOT_LENGTH)) {
			cn->is_static = true;
		}
	}
	else {
		// we are extremely unhappy ... -> malformed url
		// error(400, "URL has to start with a '/'!");
		printf("Crying game....\n");
	}
	/* chew through url, find GET, check url sanity */
	while ( !got_get && ' ' != *next ) {
		switch (*next) {
			case ' ':
				*next = '\0';
				break;
			case '?':
				got_get = 1;
				cn->pay_load = next+1;
				*next = '\0';
				break;
			case '/':
				slash_cnt++;
				if ('.' == *(next+1) && '.' == *(next+2) && '/' == *(next+3))
					slash_cnt--;
				break;
			case '.':
				if ('/' == *(next-1) && '/' != *(next+1))
					error = 400;  /* trying to reach hidden files */
				break;
			default:
				// keep chewing
				break;
		}
		next++;
	}
	/* GET - count get parameters */
	while ( got_get && ' ' != *next ) {
		switch (*next) {
			case '=':
				get_cnt++;
				break;
			default:
				// keep chewing
				break;
		}
		next++;
	}
	*next = '\0';
	next++;
	/* GET - count get parameters */
	if (0 == strncasecmp(next, "HTTP/0.9", 8)) { cn->http_prot=HTTP_09; }
	if (0 == strncasecmp(next, "HTTP/1.0", 8)) { cn->http_prot=HTTP_10; }
	if (0 == strncasecmp(next, "HTTP/1.1", 8)) { cn->http_prot=HTTP_11; }
#if DEBUG_VERBOSE==1
	printf("URL SLASHES: %d -- GET PARAMTERS: %d --ERRORS: %d --STATIC: %d\n",
		slash_cnt, get_cnt, error, cn->is_static);
#endif
	if (0 == strncasecmp(cn->url+1, FAVICON_URL, FAVICON_URL_LENGTH) ||
	    0 == strncasecmp(cn->url+1, ROBOTS_URL, ROBOTS_URL_LENGTH) ) {
		cn->is_static=true;
	}
}

static const char
*getmimetype(const char *name)
{
	/* only bother with a few mime types - let the browser figure the rest out */
	if (strstr(name, ".htm"))
		return "text/html";
	else if (strstr(name, ".css"))
		return "text/css";
	else if (strstr(name, ".js"))
		return "text/javascript";
	else if (strstr(name, ".ico"))
		return "image/vnd.microsoft.icon";
	else
		return "application/octet-stream";
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
	luaL_dofile   (L, "app/_init.lua");
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
		cn->processed_bytes = strlen(cn->data_buf_head);

		/* signal the select loop that we are done ... */
		while (REQSTATE_SEND_FILE != cn->req_state) {
			sent = send (cn->net_socket, "", 0, 0);
			cn->req_state       = REQSTATE_SEND_FILE;
		}

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
 * FIXME: less than ideal, we tonumber the socket into Lua, we shall use the
 * cn_strct as lightuserdata instead
 * @param:        the connection pointer
 * @param:        the string reference
 */
static int
l_buffer_output (lua_State *L)
{
	struct cn_strct *cn  = NULL;
	cn  =  (struct cn_strct*) lua_touserdata(L, 1);

	cn->processed_bytes = lua_strlen (L, 2);
	strncpy( cn->data_buf_head, lua_tostring (L, 2), cn->processed_bytes );

	return 0;
}
#else
/*
 * A native method that returns a static response into the connections socket
 * It's meant to be used as a test method
 */
static void
c_response ( struct cn_strct *cn )
{
	char *page = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n\
  \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\" >\n\
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\" >\n\
<head>\n\
  <title>A C-generated dynamic page</title>\n\
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
static void
show_cn (struct cn_strct *cn)
{
	printf("\n\nIDENTIFIER:  %d\n"
		"REQSTATE:  %d\n"
		"DATA_ALL: %s\n"
		"DATA_NOW: %s\n"
		"PROCESSED: %d\n",
		cn->identifier,
		cn->req_state,
		cn->data_buf_head,
		cn->data_buf,
		cn->processed_bytes);
}
#endif

// vim: ts=4 sw=4 sts=4 sta tw=80 list
