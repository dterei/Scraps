// Manage threads for the server.
//
// Simple distribution system for load among threads. A single, 'dispatcher'
// thread listens for connections and round-robbins them to the worker threads.
// Once a thread is assinged, it is never moved. Load balancing at the
// request/response level is much more involved, so we don't.
#ifndef _THREADS_H
#define _THREADS_H

#include "connections.h"

#include <event.h>

struct _conn_queue_t;

// a worker thread, handles a fixed set of connections, processing
// request/responses for them.
typedef struct _worker_thread_t {
    pthread_t thread_id;                  /* unique ID of this thread */
    struct event_base *base;              /* libevent handle this thread uses */
    struct event notify_event;            /* listen event for notify pipe */
    int notify_receive_fd;                /* receiving end of notify pipe */
    int notify_send_fd;                   /* sending end of notify pipe */
    struct _conn_queue_t *new_conn_queue; /* queue of new connections to handle */
} worker_thread_t;

// the main, intial thread that we use for listening for connections and
// dispatching them to worker threads.
typedef struct {
    pthread_t thread_id;     /* unique ID of this thread */
    struct event_base *base; /* libevent handle this thread uses */
} dispatcher_thread_t;

void thread_init(int nthreads, struct event_base *main_base);

void dispatch_conn_new(int sfd, enum conn_states init_state, int event_flags,
                       int read_buffer_size);

#endif

