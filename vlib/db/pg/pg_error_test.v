// vtest build: !windows && !musl?
module main

import db.pg

// Regression test for https://github.com/vlang/v/issues/28225
//
// When PostgreSQL is unreachable, `connect_slot` previously returned an error
// whose message was built from a dangling pointer: `PQerrorMessage` returns a
// pointer to libpq's internal buffer and `.vstring()` only wraps it zero-copy,
// then `PQfinish` freed the buffer before the message was interpolated. The
// result was a garbled error string full of null bytes / binary garbage.
fn test_connect_failure_error_is_well_formed() {
	// Port 59999 is deliberately not running a PostgreSQL server, so
	// `connect_slot` takes the `status != .ok` path that produced the
	// garbled message. This test does not require a live database.
	conn := pg.connect_direct_with_conninfo('host=localhost port=59999 user=nobody dbname=nodb') or {
		err_str := err.str()
		// The message must contain readable text and no embedded NUL bytes.
		assert err_str.contains('Connection to a PG database failed'), 'missing prefix: ${err_str}'
		assert !err_str.contains('\u0000'), 'garbled error message contains NUL bytes: ${err_str}'
		return
	}
	// An unexpected connection to a non-running server succeeded — skip.
	conn.close() or {}
}
