#include <libpq-fe.h>
#include <stddef.h>

size_t v_pg_notify_size(void) { return sizeof(PGnotify); }
size_t v_pg_notify_pid_offset(void) { return offsetof(PGnotify, be_pid); }
size_t v_pg_notify_payload_offset(void) { return offsetof(PGnotify, extra); }
size_t v_pg_notify_next_offset(void) { return offsetof(PGnotify, next); }

_Static_assert(CONNECTION_OK == 0 && CONNECTION_BAD == 1 && CONNECTION_STARTED == 2 &&
	CONNECTION_MADE == 3 && CONNECTION_AWAITING_RESPONSE == 4 && CONNECTION_AUTH_OK == 5 &&
	CONNECTION_SETENV == 6 && CONNECTION_SSL_STARTUP == 7 && CONNECTION_NEEDED == 8,
	"V libpq connection status ABI");
_Static_assert(PGRES_EMPTY_QUERY == 0 && PGRES_COMMAND_OK == 1 && PGRES_TUPLES_OK == 2 &&
	PGRES_COPY_OUT == 3 && PGRES_COPY_IN == 4 && PGRES_BAD_RESPONSE == 5 &&
	PGRES_NONFATAL_ERROR == 6 && PGRES_FATAL_ERROR == 7 && PGRES_COPY_BOTH == 8 &&
	PGRES_SINGLE_TUPLE == 9, "V libpq execution status ABI");
