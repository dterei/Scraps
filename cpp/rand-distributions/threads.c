// Manage threads for the server.

#include "connections.h"
#include "threads.h"
#include "server.h"

#include <event.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

// prototypes
static void setup_thread(worker_thread_t *t);
static void create_worker(void *(*func)(void *), void *arg);
static void *worker_entry(void *arg);
static void thread_new_conn_handler(int fd, short which, void *arg);

/* An item in the connection queue. */
typedef struct _new_conn_t {
    int sfd;                     // socket
    enum conn_states init_state; // socket starting fsm state
    int event_flags;             // event flags to apply
    struct _new_conn_t *next;
	 int read_buffer_size;
} new_conn_t;

/* A connection queue. */
typedef struct _conn_queue_t {
    new_conn_t *head;
    new_conn_t *tail;
    pthread_mutex_t lock;
} conn_queue_t;

// our main, dispatcher thread that handles new connections and passses them to
// worker threads.
static dispatcher_thread_t dispatcher_thread;

// Which thread we assigned a connection to most recently.
static int last_thread = -1;

// our various worker threads, each one deals with a fixed pool of connections,
// handling request/responses for them.
static worker_thread_t *threads;

// Number of worker threads that have finished setting themselves up.
static int init_count = 0;
static pthread_mutex_t init_lock;
static pthread_cond_t init_cond;

// Initializes a connection queue.
static conn_queue_t* conn_queue_new(void) {
	conn_queue_t *cq = malloc(sizeof(conn_queue_t));
	if (cq == NULL) {
		perror("Failed to allocate memory for connection queue");
		exit(EXIT_FAILURE);
	}
	pthread_mutex_init(&cq->lock, NULL);
	cq->head = NULL;
	cq->tail = NULL;
	return cq;
}

// Adds an item to a connection queue.
static void conn_queue_push(conn_queue_t *cq, new_conn_t *new_conn) {
	pthread_mutex_lock(&cq->lock);
	new_conn->next = NULL;
	if (cq->tail == NULL) {
		cq->head = new_conn;
	} else {
		cq->tail->next = new_conn;
	}
	cq->tail = new_conn;
	pthread_mutex_unlock(&cq->lock);
}

// Looks for an item on a connection queue, but doesn't block if there isn't
// one. Returns the item, or NULL if no item is available
static new_conn_t *conn_queue_pop(conn_queue_t *cq) {
	pthread_mutex_lock(&cq->lock);
	new_conn_t *new_conn = cq->head;
	if (new_conn != NULL) {
		cq->head = new_conn->next;
		if (cq->head == NULL) cq->tail = NULL;
	}
	pthread_mutex_unlock(&cq->lock);
	return new_conn;
}

// Returns a fresh connection queue item.
static new_conn_t *cqi_new(void) {
	// NOTE: Memcached uses a freelist here and group allocation to reducde
	// fragementation.
	new_conn_t *new_conn = malloc(sizeof(new_conn_t));
	return new_conn;
}

// Frees a connection queue item
static void new_conn_free(new_conn_t *new_conn) {
	// NOTE: Memcached uses a freelist here.
	free(new_conn);
}

// Initializes the thread subsystem, creating various worker threads.
//
// @param nthreads  Number of worker event handler threads to spawn
// @param main_base Event base for main thread
void thread_init(int nthreads, struct event_base *main_base) {
	int i;
	int power;

	pthread_mutex_init(&init_lock, NULL);
	pthread_cond_init(&init_cond, NULL);

	threads = calloc(nthreads, sizeof(worker_thread_t));
	if (!threads) {
		perror("Can't allocate thread descriptors");
		exit(1);
	}

	dispatcher_thread.base = main_base;
	dispatcher_thread.thread_id = pthread_self();

	for (i = 0; i < nthreads; i++) {
		int fds[2];
		if (pipe(fds)) {
			perror("Can't create notify pipe");
			exit(1);
		}
		threads[i].notify_receive_fd = fds[0];
		threads[i].notify_send_fd = fds[1];
		setup_thread(&threads[i]);
	}

	/* Create threads after we've done all the libevent setup. */
	for (i = 0; i < nthreads; i++) {
		create_worker(worker_entry, &threads[i]);
	}

	// Wait for all the threads to set themselves up before returning.
	pthread_mutex_lock(&init_lock);
	while (init_count < nthreads) {
		pthread_cond_wait(&init_cond, &init_lock);
	}
	pthread_mutex_unlock(&init_lock);
}

// Set up a worker thread.
static void setup_thread(worker_thread_t *t) {
	t->base = event_init();
	if (!t->base) {
		fprintf(stderr, "Can't allocate event base\n");
		exit(1);
	}

	/* Listen for notifications from other threads */
	event_set(&t->notify_event, t->notify_receive_fd,
	          EV_READ | EV_PERSIST, thread_new_conn_handler, t);
	event_base_set(t->base, &t->notify_event);

	if (event_add(&t->notify_event, 0) == -1) {
		fprintf(stderr, "Can't monitor libevent notify pipe\n");
		exit(1);
	}

	t->new_conn_queue = conn_queue_new();
}

// Creates a worker thread.
static void create_worker(void *(*func)(void *), void *arg) {
	int ret;
	pthread_t thread;
	pthread_attr_t attr;
	pthread_attr_init(&attr);

	if ((ret = pthread_create(&thread, &attr, func, arg)) != 0) {
		fprintf(stderr, "Can't create thread: %s\n", strerror(ret));
		exit(1);
	}
}

// Any per-thread setup can happen here; thread_init() will block until
// all threads have finished initializing.
static void register_thread_initialized(void) {
    pthread_mutex_lock(&init_lock);
    init_count++;
    pthread_cond_signal(&init_cond);
    pthread_mutex_unlock(&init_lock);
}

// Worker thread: main entry point.
static void *worker_entry(void *arg) {
	worker_thread_t *me = arg;
	register_thread_initialized();

	// enter the event loop
	event_base_loop(me->base, 0);
	return NULL;
}

// Handles a new connection event for a particular thread. Event notified
// through a threads libevent wakeup pipe.
static void thread_new_conn_handler(int fd, short which, void *arg) {
	worker_thread_t *me = arg;
	new_conn_t *new_conn;
	char buf[1];

	if (read(fd, buf, 1) != 1) {
		if (config.verbose > 0) {
			fprintf(stderr, "Can't read from libevent pipe\n");
		}
	}

	switch (buf[0]) {
	case 'c':
		new_conn = conn_queue_pop(me->new_conn_queue);
		if (NULL != new_conn) {
			conn *c = conn_new(new_conn->sfd, new_conn->init_state,
			                   new_conn->event_flags, new_conn->read_buffer_size,
			                   me->base);
			if (c == NULL) {
				if (config.verbose > 0) {
					fprintf(stderr, "Can't listen for events on fd %d\n",
					        new_conn->sfd);
				}
				close(new_conn->sfd);
			} else {
				c->thread = me;
			}
			new_conn_free(new_conn);
		}
		break;
	}
}

// Dispatches a new connection to another thread. This is only ever called from
// the main thread because of an incoming connection.
void dispatch_conn_new(int sfd, enum conn_states init_state, int event_flags,
                       int read_buffer_size) {
	new_conn_t *new_conn = cqi_new();

	last_thread = (last_thread + 1) % config.threads;
	worker_thread_t *thread = threads + last_thread;

	new_conn->sfd = sfd;
	new_conn->init_state = init_state;
	new_conn->event_flags = event_flags;
	new_conn->read_buffer_size = read_buffer_size;

	// dispatch connection
	conn_queue_push(thread->new_conn_queue, new_conn);
	char buf[1] = {'c'};
	if (write(thread->notify_send_fd, buf, 1) != 1) {
		perror("Writing to thread notify pipe");
	}
}

