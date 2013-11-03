// Protocol of memcached.
#ifndef _PROTOCOL_H
#define _PROTOCOL_H

#include <stdbool.h>
#include <unistd.h>

typedef struct token_s {
    char *value;
    size_t length;
} token_t;

#define COMMAND_TOKEN 0
#define SUBCOMMAND_TOKEN 1
#define KEY_TOKEN 1

// max tokens a memcached request can have.
#define MAX_TOKENS 8

#define NREAD_ADD 1
#define NREAD_SET 2
#define NREAD_REPLACE 3
#define NREAD_APPEND 4
#define NREAD_PREPEND 5
#define NREAD_CAS 6

// max length of a key.
#define KEY_MAX_LENGTH 250

size_t tokenize_command(char *command, token_t *tokens, const size_t max_tokens);
bool parse_command(conn *c, char *command);


#endif

