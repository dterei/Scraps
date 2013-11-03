#include "connections.h"
#include "protocol.h"
#include "server.h"

#include <assert.h>
#include <string.h>
#include <unistd.h>

/*
 * Tokenize the command string by replacing whitespace with '\0' and update
 * the token array tokens with pointer to start of each token and length.
 * Returns total number of tokens.  The last valid token is the terminal
 * token (value points to the first unprocessed character of the string and
 * length zero).
 *
 * Usage example:
 *
 *  while(tokenize_command(command, ncommand, tokens, max_tokens) > 0) {
 *      for(int ix = 0; tokens[ix].length != 0; ix++) {
 *          ...
 *      }
 *      ncommand = tokens[ix].value - command;
 *      command  = tokens[ix].value;
 *   }
 */
size_t tokenize_command(char *command, token_t *tokens, const size_t max_tokens) {
	char *s, *e;
	size_t ntokens = 0;
	size_t len = strlen(command);
	unsigned int i = 0;

	assert(command != NULL && tokens != NULL && max_tokens > 1);

	s = e = command;
	for (i = 0; i < len; i++) {
		if (*e == ' ') {
			if (s != e) {
				tokens[ntokens].value = s;
				tokens[ntokens].length = e - s;
				ntokens++;
				*e = '\0';
				if (ntokens == max_tokens - 1) {
					e++;
					s = e; /* so we don't add an extra token */
					break;
				}
			}
			s = e + 1;
		}
		e++;
	}

	if (s != e) {
		tokens[ntokens].value = s;
		tokens[ntokens].length = e - s;
		ntokens++;
	}

	// If we scanned the whole string, the terminal value pointer is null,
	// otherwise it is the first unprocessed character.
	tokens[ntokens].value =  *e == '\0' ? NULL : e;
	tokens[ntokens].length = 0;
	ntokens++;

	return ntokens;
}

// parse a single memcached request.
bool parse_command(conn *c, char *command) {
	token_t tokens[MAX_TOKENS];
	size_t ntokens;
	int comm;

	assert(c != NULL);

	ntokens = tokenize_command(command, tokens, MAX_TOKENS);
	if (ntokens >= 3 &&
			((strcmp(tokens[COMMAND_TOKEN].value, "get") == 0) ||
			(strcmp(tokens[COMMAND_TOKEN].value, "bget") == 0))) {
		process_get_command(c, tokens, ntokens, false);

	} else if (ntokens >= 3 &&
			(strcmp(tokens[COMMAND_TOKEN].value, "gets") == 0)) {
		process_get_command(c, tokens, ntokens, true);

	/* } else if ((ntokens == 6 || ntokens == 7) && */
	/* 		((strcmp(tokens[COMMAND_TOKEN].value, "add") == 0 && (comm = NREAD_ADD)) || */
	/* 		(strcmp(tokens[COMMAND_TOKEN].value, "set") == 0 && (comm = NREAD_SET)) || */
	/* 		(strcmp(tokens[COMMAND_TOKEN].value, "replace") == 0 && (comm = NREAD_REPLACE)) || */
	/* 		(strcmp(tokens[COMMAND_TOKEN].value, "prepend") == 0 && (comm = NREAD_PREPEND)) || */
	/* 		(strcmp(tokens[COMMAND_TOKEN].value, "append") == 0 && (comm = NREAD_APPEND)) )) { */
	/* 	process_update_command(c, tokens, ntokens, comm, false); */

	/* } else if ((ntokens == 7 || ntokens == 8) && */
	/* 		(strcmp(tokens[COMMAND_TOKEN].value, "cas") == 0 && (comm = NREAD_CAS))) { */
	/* 	process_update_command(c, tokens, ntokens, comm, true); */

	/* } else if ((ntokens == 4 || ntokens == 5) && */
	/* 		(strcmp(tokens[COMMAND_TOKEN].value, "incr") == 0)) { */
	/* 	process_arithmetic_command(c, tokens, ntokens, 1); */

	/* } else if ((ntokens == 4 || ntokens == 5) && */
	/* 		(strcmp(tokens[COMMAND_TOKEN].value, "decr") == 0)) { */
	/* 	process_arithmetic_command(c, tokens, ntokens, 0); */

	/* } else if (ntokens >= 3 && ntokens <= 5 && */
	/* 		(strcmp(tokens[COMMAND_TOKEN].value, "delete") == 0)) { */
	/* 	process_delete_command(c, tokens, ntokens); */

	/* } else if ((ntokens == 4 || ntokens == 5) && */
	/* 		(strcmp(tokens[COMMAND_TOKEN].value, "touch") == 0)) { */
	/* 	process_touch_command(c, tokens, ntokens); */

	/* } else if (ntokens >= 2 && */
	/* 		(strcmp(tokens[COMMAND_TOKEN].value, "stats") == 0)) { */
	/* 	process_stat(c, tokens, ntokens); */

	/* } else if (ntokens >= 2 && ntokens <= 4 && */
	/* 		(strcmp(tokens[COMMAND_TOKEN].value, "flush_all") == 0)) { */
	/* 	process_flush_cmd(c, tokens, ntokens); */

	/* } else if (ntokens == 2 && */
	/* 		(strcmp(tokens[COMMAND_TOKEN].value, "version") == 0)) { */
	/* 	process_version_cmd(c, tokens, ntokens); */

	/* } else if (ntokens == 2 && */
	/* 		(strcmp(tokens[COMMAND_TOKEN].value, "quit") == 0)) { */
	/* 	process_quite_cmd(c, tokens, ntokens); */

	/* } else if (ntokens == 2 && */
	/* 		(strcmp(tokens[COMMAND_TOKEN].value, "shutdown") == 0)) { */
	/* 	process_shutdown_cmd(c, tokens, ntokens); */

	/* } else if (ntokens > 1 && */
	/* 		(strcmp(tokens[COMMAND_TOKEN].value, "slabs") == 0)) { */
	/* 	process_slab_cmd(c, tokens, ntokens); */

	/* } else if ((ntokens == 3 || ntokens == 4) && */
	/* 		(strcmp(tokens[COMMAND_TOKEN].value, "verbosity") == 0)) { */
	/* 	process_verbosity_command(c, tokens, ntokens); */

	} else {
		return false;
	}

	return true;
}

