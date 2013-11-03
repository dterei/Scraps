// Memcached fake, benchmarking server.
#include "connections.h"
#include "fsm.h"
#include "items.h"
#include "protocol.h"
#include "server.h"
#include "settings.h"
#include "threads.h"

#include <assert.h>
#include <errno.h>
#include <event.h>
#include <fcntl.h>
#include <netinet/tcp.h>
#include <pthread.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sysexits.h>
#include <unistd.h>

#define REQ_PER_EVENT 20

// possible results reading from network.
typedef enum {
    READ_DATA_RECEIVED,
    READ_NO_DATA_RECEIVED,
    READ_ERROR,
    READ_MEMORY_ERROR // out-of-memory
} read_result;

// possible results writing to network.
typedef enum {
	TRANSMIT_COMPLETE,   // All done writing.
	TRANSMIT_INCOMPLETE, // More data remaining to write.
	TRANSMIT_SOFT_ERROR, // Can't write any more right now.
	TRANSMIT_HARD_ERROR  // Can't write (c->state is set to conn_closing).
} write_result;

// prototypes
static int server_socket(int port);
static void drive_machine(conn *c);
static void reset_cmd_handler(conn *c);
static read_result read_network(conn *c);
static int read_command(conn *c);
static void process_command(conn *c, char *command);
static write_result transmit(conn *c);
static void out_string(conn *c, const char *str);

settings config;
static conn *listen_conn = NULL;
static struct event_base *main_base;

// SIGINT signal handler.
static void sig_handler(const int sig) {
    printf("SIGINT handled.\n");
    exit(EXIT_SUCCESS);
}

// launch the server.
int main (int argc, char **argv) {
   int retval = EXIT_SUCCESS;

	signal(SIGINT, sig_handler);
	setbuf(stderr, NULL);

	// ignore SIGPIPE signals; we can use errno == EPIPE if we
	// need that information
	if (sigignore(SIGPIPE) == -1) {
		perror("failed to ignore SIGPIPE; sigaction");
		exit(EX_OSERR);
	}
	
	config = settings_init();
	int r = settings_parse(argc, argv, &config);
	if (r) return r;

	/* initialize main thread libevent instance */
	main_base = event_init();

	/* initialize other stuff */
	conn_init();

	/* start up worker threads */
	thread_init(config.threads, main_base);

    /* create the listening socket, bind it, and init */
	errno = 0;
	if (server_socket(config.tcpport)) {
		fprintf(stderr, "failed to listen on TCP port %d", config.tcpport);
		exit(EX_OSERR);
	}

	// Give the sockets a moment to open. I know this is dumb, but the error is
	// only an advisory.
	usleep(1000);

	/* enter the event loop */
	if (event_base_loop(main_base, 0) != 0) {
		retval = EXIT_FAILURE;
	}

	return retval;
}

// create a new socket set in non-blocking mode.
static int new_socket(struct addrinfo *ai) {
	int sfd; int flags;

	if ((sfd = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol)) == -1) {
		return -1;
	}

	if ((flags = fcntl(sfd, F_GETFL, 0)) < 0 ||
		fcntl(sfd, F_SETFL, flags | O_NONBLOCK) < 0) {
		perror("setting O_NONBLOCK"); close(sfd); return -1;
	}
	return sfd;
}

// Create a socket and bind it to a specific port number
static int server_socket(int port) {
    int sfd;
    struct linger ling = {0, 0};
    struct addrinfo *ai;
    struct addrinfo *next;
    struct addrinfo hints = { .ai_flags = AI_PASSIVE,
                              .ai_family = AF_UNSPEC,
										.ai_socktype = SOCK_STREAM };
    char port_buf[NI_MAXSERV];
    int error;
    int success = 0;
    int flags =1;

    if (port == -1) {
        port = 0;
    }

    snprintf(port_buf, sizeof(port_buf), "%d", port);
    error = getaddrinfo(NULL, port_buf, &hints, &ai);
    if (error != 0) {
        if (error != EAI_SYSTEM) {
          fprintf(stderr, "getaddrinfo(): %s\n", gai_strerror(error));
		  } else {
          perror("getaddrinfo()");
		  }
		  return 1;
    }

	for (next = ai; next; next = next->ai_next) {
		conn *c;
		if ((sfd = new_socket(next)) == -1) {
			/* getaddrinfo can return "junk" addresses,
			* we make sure at least one works before erroring.
			*/
			if (errno == EMFILE) {
				/* ...unless we're out of fds */
				perror("server_socket");
				exit(EX_OSERR);
			}
			continue;
		}

		setsockopt(sfd, SOL_SOCKET, SO_REUSEADDR, (void *)&flags, sizeof(flags));
		error = setsockopt(sfd, SOL_SOCKET, SO_KEEPALIVE, (void *)&flags, sizeof(flags));
		if (error != 0) perror("setsockopt");
		error = setsockopt(sfd, SOL_SOCKET, SO_LINGER, (void *)&ling, sizeof(ling));
		if (error != 0) perror("setsockopt");
		error = setsockopt(sfd, IPPROTO_TCP, TCP_NODELAY, (void *)&flags, sizeof(flags));
		if (error != 0) perror("setsockopt");

		if (bind(sfd, next->ai_addr, next->ai_addrlen) == -1) {
			if (errno != EADDRINUSE) {
				perror("bind()");
				close(sfd);
				freeaddrinfo(ai);
				return 1;
			}
			close(sfd);
			continue;
		}

		success++;
		if (listen(sfd, 1024) == -1) {
			perror("listen()");
			close(sfd);
			freeaddrinfo(ai);
			return 1;
		}

		if (!(c = conn_new(sfd, conn_listening,
		                   EV_READ | EV_PERSIST,
		                   1, main_base))) {
			fprintf(stderr, "failed to create listening connection\n");
			exit(EXIT_FAILURE);
		}
		c->next = listen_conn;
		listen_conn = c;
	}

	freeaddrinfo(ai);

	/* Return zero iff we detected no errors in starting up connections */
	return success == 0;
}

// event loop entry point.
void event_handler(const int fd, const short which, void *arg) {
	conn *c;
	c = (conn *)arg;
	assert(c != NULL);
	c->which = which;

	/* sanity */
	if (fd != c->sfd) {
		if (config.verbose > 0) {
			fprintf(stderr, "Catastrophic: event fd doesn't match conn fd!\n");
		}
		conn_close(c);
		return;
	}

	drive_machine(c);

	/* wait for next event */
	return;
}

// state machine driver for handling an event.
static void drive_machine(conn *c) {
	int stop = 0;
	int sfd, flags = 1;
	socklen_t addrlen;
	struct sockaddr_storage addr;
	int nreqs = REQ_PER_EVENT;
	int res;
	const char *str;

	assert(c != NULL);

	while (!stop) {
		switch(c->state) {

		case conn_listening:
			addrlen = sizeof(addr);
			if ((sfd = accept(c->sfd, (struct sockaddr *)&addr, &addrlen)) == -1) {
				if (errno == EAGAIN || errno == EWOULDBLOCK) {
					/* these are transient, so don't log anything */
				} else if (errno == EMFILE) {
					if (config.verbose > 0)
						fprintf(stderr, "Too many open connections\n");
				} else {
					perror("accept()");
				}
				stop = 1;
				break;
			}
			if ((flags = fcntl(sfd, F_GETFL, 0)) < 0 ||
			    fcntl(sfd, F_SETFL, flags | O_NONBLOCK) < 0) {
				perror("setting O_NONBLOCK");
				close(sfd);
				break;
			}

			dispatch_conn_new(sfd, conn_new_cmd, EV_READ | EV_PERSIST,
			                  DATA_BUFFER_SIZE);
			stop = 1;
			break;

		case conn_new_cmd:
			// Only process nreqs at a time to avoid starving other connections
			--nreqs;
			if (nreqs >= 0) {
				reset_cmd_handler(c);
			} else {
				if (c->rbytes > 0) {
					// We have already read in data into the input buffer, so
					// libevent will most likely not signal read events on the
					// socket (unless more data is available. As a hack we should
					// just put in a request to write data, because that should be
					// possible ;-)
					if (!conn_update_event(c, EV_WRITE | EV_PERSIST)) {
						conn_set_state(c, conn_closing);
						break;
					}
				}
				stop = 1;
			}
			break;

			case conn_waiting:
				if (!conn_update_event(c, EV_READ | EV_PERSIST)) {
					conn_set_state(c, conn_closing);
					break;
				}
				conn_set_state(c, conn_read);
				stop = 1;
				break;

			case conn_read:
				res = read_network(c);
				switch (res) {
				case READ_NO_DATA_RECEIVED:
					conn_set_state(c, conn_waiting);
					break;
				case READ_DATA_RECEIVED:
					conn_set_state(c, conn_parse_cmd);
					break;
				case READ_ERROR:
					conn_set_state(c, conn_closing);
					break;
				case READ_MEMORY_ERROR:
					c->rbytes = 0; /* ignore what we read */
					out_string(c, "SERVER_ERROR out of memory reading request");
					c->after_write = conn_closing;
					break;
				}
				break;

			case conn_parse_cmd :
				if (read_command(c) == 0) {
					/* wee need more data! */
					conn_set_state(c, conn_waiting);
				}
				break;

			case conn_write:
				// We want to write out a simple response. If we haven't already,
				// assemble it into a msgbuf list (this will be a single-entry list
				// for TCP).
            if (c->iovused == 0) {
					if (!conn_add_iov(c, c->wcurr, c->wbytes)) {
						if (config.verbose > 0) {
							fprintf(stderr, "Couldn't build response\n");
						}
						conn_set_state(c, conn_closing);
						break;
					}
				}
            /* fall through... */

			case conn_mwrite:
				switch (transmit(c)) {
				case TRANSMIT_COMPLETE:
					if (c->state == conn_mwrite) {
						while (c->ileft > 0) {
							// release the item.
							item *it = *(c->icurr);
							item_remove(it);
							c->icurr++;
							c->ileft--;
						}
						conn_set_state(c, conn_new_cmd);
					} else if (c->state == conn_write) {
						conn_set_state(c, c->after_write);
					} else {
						if (config.verbose > 0) {
							fprintf(stderr, "Unexpected state %d\n", c->state);
						}
						conn_set_state(c, conn_closing);
					}
					break;

				case TRANSMIT_INCOMPLETE:
				case TRANSMIT_HARD_ERROR:
					break; /* Continue in state machine. */

				case TRANSMIT_SOFT_ERROR:
					stop = true;
					break;
				}
				break;

		case conn_closing:
			conn_close(c);
			stop = true;
			break;

		case conn_min_state:
		case conn_max_state:
			assert(false);
			break;
		}
	}
}

// reset a connection to a ready-state for processing a new request.
static void reset_cmd_handler(conn *c) {
	c->cmd = -1;
	conn_shrink(c);
	if (c->rbytes > 0) {
		conn_set_state(c, conn_parse_cmd);
	} else {
		conn_set_state(c, conn_waiting);
	}
}

// read from network as much as we can, handle buffer overflow and connection
// close. before reading, move the remaining incomplete fragment of a command
// (if any) to the beginning of the buffer.
// 
// To protect us from someone flooding a connection with bogus data causing the
// connection to eat up all available memory, break out and start looking at
// the data I've got after a number of reallocs...
static read_result read_network(conn *c) {
	read_result ret = READ_NO_DATA_RECEIVED;
	int res;
	#define MAX_ALLOCS 4
	int num_allocs = 0;
	assert(c != NULL);

	if (c->rcurr != c->rbuf) {
		if (c->rbytes != 0) {
			memmove(c->rbuf, c->rcurr, c->rbytes);
		}
		c->rcurr = c->rbuf;
	}

	while (1) {
		if (c->rbytes >= c->rsize) {
			if (num_allocs == MAX_ALLOCS) {
				return ret;
			}
			++num_allocs;
			char *new_rbuf = realloc(c->rbuf, c->rsize * 2);
			if (!new_rbuf) {
				if (config.verbose > 0) {
					fprintf(stderr, "Couldn't realloc input buffer\n");
				}
				return READ_MEMORY_ERROR;
			}
			c->rcurr = c->rbuf = new_rbuf;
			c->rsize *= 2;
		}

		int avail = c->rsize - c->rbytes;
		res = read(c->sfd, c->rbuf + c->rbytes, avail);
		if (res > 0) {
			ret = READ_DATA_RECEIVED;
			c->rbytes += res;
			if (res < avail) {
				break; // all data read
			} else {
				continue; // still more on wire
			}
		} else if (res == 0) {
			return READ_ERROR;
		} else if (res == -1) {
			if (errno == EAGAIN || errno == EWOULDBLOCK) {
				break;
			}
			return READ_ERROR;
		}
	}
	return ret;
}

// if we have a complete line in the buffer, process it.
// @return 0 when more data is needed, 1 otherwise.
static int read_command(conn *c) {
	assert(c != NULL);
	assert(c->rcurr <= (c->rbuf + c->rsize));
	assert(c->rbytes > 0);

	char *el, *cont;

	if (c->rbytes == 0) return 0;

	// find end-of-line.
	el = memchr(c->rcurr, '\n', c->rbytes);

	// not found.
	if (!el) {
		if (c->rbytes > 1024) {
			// We didn't have a '\n' in the first k.
			char *ptr = c->rcurr;
			while (*ptr == ' ') ++ptr;
			// This _has_ to be a large multiget, if not we should just nuke the
			// connection.
			if (ptr - c->rcurr > 100 ||
			    (strncmp(ptr, "get ", 4) && strncmp(ptr, "gets ", 5))) {
				conn_set_state(c, conn_closing);
				return 1;
			}
		}
		return 0;
	}

	// found end-of-line.
	cont = el + 1;
	if ((el - c->rcurr) > 1 && *(el - 1) == '\r') {
		el--;
	}
	*el = '\0';

	// process the command.
	assert(cont <= (c->rcurr + c->rbytes));
	process_command(c, c->rcurr);
	c->rbytes -= (cont - c->rcurr);
	c->rcurr = cont;
	assert(c->rcurr <= (c->rbuf + c->rsize));
	return 1;
}

// parse a single memcached request.
static void process_command(conn *c, char *command) {
	assert(c != NULL);

	if (config.verbose > 1) {
		fprintf(stderr, "<%d %s\n", c->sfd, command);
	}

	// for commands set/add/replace, we build an item and read the data directly
	// into it, then continue in nread_complete().

	c->msgcurr = 0;
	c->msgused = 0;
	c->iovused = 0;
	if (!conn_add_msghdr(c) != 0) {
		out_string(c, "SERVER_ERROR out of memory preparing response");
		return;
	}

	// parse_command also handles dispatching it.
	if (!parse_command(c, command)) {
		out_string(c, "ERROR");
	}
}

// process a memcached get(s) command. (we don't support CAS).
inline void process_get_command(conn *c, token_t *tokens, size_t ntokens,
                                bool return_cas) {
	char *key;
	size_t nkey;
	int i = 0;
	item *it;
	token_t *key_token = &tokens[KEY_TOKEN];
	char *suffix;

	assert(c != NULL);

	// process the whole command line, (only part of it may be tokenized right now)
	do {
		// process all tokenized keys at this stage.
		while(key_token->length != 0) {
			key = key_token->value;
			nkey = key_token->length;

			if(nkey > KEY_MAX_LENGTH) {
				out_string(c, "CLIENT_ERROR bad command line format");
				return;
			}

			// lookup key-value.
			it = item_get(key, nkey);
			
			// hit.
			if (it) {
				if (i >= c->isize && !conn_expand_items(c)) {
					item_remove(it);
					break;
				}

				// Construct the response. Each hit adds three elements to the
				// outgoing data list:
				//   "VALUE <key> <flags> <data_length>\r\n"
				//   "<data>\r\n"
				// The <data> element is stored on the connection item list, not on
				// the iov list.
				if (!conn_add_iov(c, "VALUE ", 6) != 0 ||
				    !conn_add_iov(c, ITEM_key(it), it->nkey) != 0 ||
				    !conn_add_iov(c, ITEM_suffix(it), it->nsuffix + it->nbytes) != 0) {
					item_remove(it);
					break;
				}

				if (config.verbose > 1) {
					fprintf(stderr, ">%d sending key %s\n", c->sfd, key);
				}

				// add item to remembered list (i.e., we've taken ownership of them
				// through refcounting and later must release them once we've
				// written out the iov associated with them).
				item_update(it);
				*(c->ilist + i) = it;
				i++;
			}

			key_token++;
		}

		/*
		 * If the command string hasn't been fully processed, get the next set
		 * of tokens.
		 */
		if(key_token->value != NULL) {
			ntokens = tokenize_command(key_token->value, tokens, MAX_TOKENS);
			key_token = tokens;
		}

	} while(key_token->value != NULL);

	c->icurr = c->ilist;
	c->ileft = i;

	if (config.verbose > 1) {
		fprintf(stderr, ">%d END\n", c->sfd);
	}

	// If the loop was terminated because of out-of-memory, it is not reliable
	// to add END\r\n to the buffer, because it might not end in \r\n. So we
	// send SERVER_ERROR instead.
	if (key_token->value != NULL || !conn_add_iov(c, "END\r\n", 5) != 0) {
		out_string(c, "SERVER_ERROR out of memory writing get response");
	} else {
		conn_set_state(c, conn_mwrite);
		c->msgcurr = 0;
	}
}

// Transmit the next chunk of data from our list of msgbuf structures.
// 
// Returns:
//   TRANSMIT_COMPLETE   All done writing.
//   TRANSMIT_INCOMPLETE More data remaining to write.
//   TRANSMIT_SOFT_ERROR Can't write any more right now.
//   TRANSMIT_HARD_ERROR Can't write (c->state is set to conn_closing)
static write_result transmit(conn *c) {
	assert(c != NULL);

	if (c->msgcurr < c->msgused &&
	    c->msglist[c->msgcurr].msg_iovlen == 0) {
		/* Finished writing the current msg; advance to the next. */
		c->msgcurr++;
	}

	if (c->msgcurr < c->msgused) {
		ssize_t res;
		struct msghdr *m = &c->msglist[c->msgcurr];

		res = sendmsg(c->sfd, m, 0);
		if (res > 0) {
			// We've written some of the data. Remove the completed iovec entries
			// from the list of pending writes.
			while (m->msg_iovlen > 0 && res >= m->msg_iov->iov_len) {
				res -= m->msg_iov->iov_len;
				m->msg_iovlen--;
				m->msg_iov++;
			}

			// Might have written just part of the last iovec entry; adjust it so
			// the next write will do the rest.
			if (res > 0) {
				m->msg_iov->iov_base = (caddr_t)m->msg_iov->iov_base + res;
				m->msg_iov->iov_len -= res;
			}
			return TRANSMIT_INCOMPLETE;

		} else if (res == -1 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
			if (!conn_update_event(c, EV_WRITE | EV_PERSIST)) {
				if (config.verbose > 0) {
					fprintf(stderr, "Couldn't update event\n");
				}
				conn_set_state(c, conn_closing);
				return TRANSMIT_HARD_ERROR;
			}
			return TRANSMIT_SOFT_ERROR;

		} else {
			// if res == 0 or res == -1 and error is not EAGAIN or EWOULDBLOCK, we
			// have a real error, on which we close the connection.
			if (config.verbose > 0) {
				perror("Failed to write, and not due to blocking");
			}
			conn_set_state(c, conn_closing);
			return TRANSMIT_HARD_ERROR;
		}
	} else {
		return TRANSMIT_COMPLETE;
	}
}

// write out a string to the connection, clearing an existing pending data.
// This is usually used for error cases.
static void out_string(conn *c, const char *str) {
	size_t len;

	assert(c != NULL);

	if (config.verbose > 1) fprintf(stderr, ">%d %s\n", c->sfd, str);

	/* Nuke a partial output... */
	c->msgcurr = 0;
	c->msgused = 0;
	c->iovused = 0;
	conn_add_msghdr(c);

	len = strlen(str);
	if ((len + 2) > c->wsize) {
		/* ought to be always enough. just fail for simplicity */
		str = "SERVER_ERROR output line too long";
		len = strlen(str);
	}

	memcpy(c->wbuf, str, len);
	memcpy(c->wbuf + len, "\r\n", 2);
	c->wbytes = len + 2;
	c->wcurr = c->wbuf;

	conn_set_state(c, conn_write);
	c->after_write = conn_new_cmd;
	return;
}

