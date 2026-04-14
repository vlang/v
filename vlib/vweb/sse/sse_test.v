module sse

import vweb

struct TestApp {
	vweb.Context
}

fn test_start_returns_an_error_for_missing_vweb_connection() {
	app := &TestApp{}
	mut stream := new_connection(app.conn)
	stream.start() or {
		assert err.msg().contains('SSE connection is not available')
		return
	}
	assert false
}

fn test_send_message_returns_an_error_for_missing_vweb_connection() {
	app := &TestApp{}
	mut stream := new_connection(app.conn)
	stream.send_message(SSEMessage{
		data: 'ping'
	}) or {
		assert err.msg().contains('SSE connection is not available')
		return
	}
	assert false
}
