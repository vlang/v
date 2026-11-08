module httptest

import net.http
import time

// TestServer is a running HTTP server bound to 127.0.0.1 on an
// ephemeral port, for use in tests. It mirrors Go's httptest.Server.
pub struct TestServer {
pub mut:
	server &http.Server
pub:
	url string
}

// new_server starts a test server with `handler` and waits until it
// accepts connections, returning the server and its base URL.
pub fn new_server(handler http.Handler) !TestServer {
	mut s := &http.Server{
		addr:                 '127.0.0.1:0'
		handler:              handler
		worker_num:           1
		accept_timeout:       50 * time.millisecond
		show_startup_message: false
	}
	_ := spawn s.listen_and_serve()
	s.wait_till_running()!
	time.sleep(50 * time.millisecond)
	return TestServer{
		server: s
		url:    'http://${s.addr}'
	}
}

// close shuts the test server down.
pub fn (mut t TestServer) close() {
	t.server.close()
}
