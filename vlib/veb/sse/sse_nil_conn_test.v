// Coverage for the guard branches of `veb.sse`, which `sse_test.v` cannot reach:
// it drives a real TCP server, so it always has a connection. Everything here
// runs against a bare `veb.Context` with `conn == nil`, so nothing writes to a
// socket and nothing needs a listener.
//
// Note: `start_connection` prints a warning to stderr on this path, so the test
// log will contain one line of expected noise per call.
import veb
import veb.sse

struct SseNilContext {
	veb.Context
}

fn sse_nil_conn(mut ctx SseNilContext) &sse.SSEConnection {
	return sse.start_connection(mut ctx.Context)
}

fn test_start_connection_without_a_tcp_conn_reports_nil() {
	mut ctx := SseNilContext{}
	mut conn := sse_nil_conn(mut ctx)
	assert unsafe { conn.conn } == unsafe { nil }
}

fn test_send_message_without_a_tcp_conn_is_an_error() {
	mut ctx := SseNilContext{}
	mut conn := sse_nil_conn(mut ctx)
	// Every field arrangement reaches the same nil check first.
	conn.send_message() or {
		assert err.msg() == 'SSE connection is not available (no TCP connection)'
		return
	}
	assert false, 'send_message should have failed without a connection'
}

fn test_send_message_with_fields_but_no_tcp_conn_is_an_error() {
	mut ctx := SseNilContext{}
	mut conn := sse_nil_conn(mut ctx)
	conn.send_message(id: '1', event: 'update', data: 'payload', retry: 3000) or {
		assert err.msg() == 'SSE connection is not available (no TCP connection)'
		return
	}
	assert false, 'send_message should have failed without a connection'
}

fn test_close_without_a_tcp_conn_is_a_no_op() {
	mut ctx := SseNilContext{}
	mut conn := sse_nil_conn(mut ctx)
	// close() swallows its own errors, so this must simply return.
	conn.close()
	assert unsafe { conn.conn } == unsafe { nil }
}

fn test_close_then_send_message_is_still_the_same_error() {
	mut ctx := SseNilContext{}
	mut conn := sse_nil_conn(mut ctx)
	conn.close()
	conn.send_message(data: 'after-close') or {
		assert err.msg() == 'SSE connection is not available (no TCP connection)'
		return
	}
	assert false, 'send_message should have failed without a connection'
}

fn test_sse_message_defaults_are_all_empty() {
	// The handler only emits a field when it is non-zero, so a zero message
	// serialises to just the terminating blank line.
	m := sse.SSEMessage{}
	assert m.id == ''
	assert m.event == ''
	assert m.data == ''
	assert m.retry == 0
}
