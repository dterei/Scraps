// Server connection handling
#ifndef _CONNECTIONS_H
#define _CONNECTIONS_H

#include "fsm.h"
#include "items.h"

#include <event.h>
#include <stdbool.h>

// size of initial read/write buffers for request/responses.
#define DATA_BUFFER_SIZE 2048
// Initial size of the sendmsg() scatter/gather array.
#define IOV_LIST_INITIAL 400
// Initial number of sendmsg() argument structures to allocate.
#define MSG_LIST_INITIAL 10
// Initial size of list of items being returned by "get".
#define ITEM_LIST_INITIAL 200
// Initial TCP packet max size (for handling MTU gracefully).
#define MAX_PAYLOAD_SIZE 1400

// Representation of a connection. Used for both client connections and our
// listening socket.
typedef struct _conn {
	struct _conn *next;              // allow link-listing of connections.

	int sfd;                         // underlying socket.
	struct event event;
	short ev_flags;
	struct _worker_thread_t *thread; // thread managing this connection.

	enum conn_states state;          // fsm state.
	enum conn_states after_write;    // fsm state to transition to after writing wbuf.
	short which;                     // which event generate the event.
	int cmd;                         // which memcached cmd are we processing.

	// [..........|...........^...................]
	// ^          ^                               ^
	// rbuf       rcurr       rcurr+rbytes        rbuf+rsize
	//
	int  rsize;                      // size of rbuf.
	char *rbuf;                      // buffer to read commands into.
	char *rcurr;                     // pointer into rbuf to end of parsed data.
	int  rbytes;                     // how much data, starting from rcur, do we have unparsed.

	// [..........|...........^...................]
	// ^          ^                               ^
	// wbuf       wcurr       wcurr+wbytes        wbuf+wsize
	//
	int  wsize;                      // size of wbuf.
	char *wbuf;                      // buffer for data to write to network.
	char *wcurr;                     // pointer into wbuf to end of written data.
	int  wbytes;                     // remaining data in wbuf not yet written.

	struct iovec *iov;
	int    iovsize;   // number of elements allocated in iov[].
	int    iovused;   // number of elements used in iov[].

	struct msghdr *msglist;
	int    msgsize;   // number of elements allocated in msglist[].
	int    msgused;   // number of elements used in msglist[].
	int    msgcurr;   // element in msglist[] being transmitted now.
	int    msgbytes;  // number of bytes in current msg.

	item   **ilist; // list of items to write out.
	int    isize;   // number of elements in ilist.
	item   **icurr; // current free slot in ilist.
	int    ileft;   // space left in ilist for allocaiton.
} conn;

// new connection management.
void conn_init(void);
conn *conn_new(const int sfd,
               enum conn_states init_state,
               const int event_flags,
               const int read_buffer_size,
               struct event_base *base);
void conn_free(conn *c);
int conn_add_to_freelist(conn *c);
void conn_close(conn *c);

// in-flight connection management.
void conn_set_state(conn *c, enum conn_states state);
int conn_update_event(conn *c, const int new_flags);
void conn_shrink(conn *c);
int conn_add_msghdr(conn *c);
bool conn_add_iov(conn *c, const void *buf, int len);
bool conn_expand_items(conn *c);

#endif

