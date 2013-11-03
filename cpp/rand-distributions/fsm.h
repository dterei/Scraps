// Encoding of state machine for event model of connections.
#ifndef _FSM_H
#define _FSM_H

// State machine for connections
enum conn_states {
	conn_min_state,
	conn_listening,
	conn_new_cmd,
	conn_waiting,
	conn_read,
	conn_parse_cmd,
	conn_write,
	conn_mwrite,
	conn_closing,
	conn_max_state
};

// Convert a state name to a human readable form.
static const char *state_text(enum conn_states state) {
	const char* const statenames[] = { "conn_min_state",
	                                   "conn_listening",
                                      "conn_new_cmd",
                                      "conn_waiting",
                                      "conn_read",
                                      "conn_parse_cmd",
                                      "conn_write",
                                      "conn_mwrite",
                                      "conn_closing"};
	return statenames[state];
}

#endif

