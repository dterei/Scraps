// Memcached fake, benchmarking server.
#ifndef _SERVER_H
#define _SERVER_H

#include "protocol.h"
#include "settings.h"

extern settings config;
void event_handler(const int fd, const short which, void *arg);
inline void process_get_command(conn *c, token_t *tokens, size_t ntokens, bool return_cas);

#endif

