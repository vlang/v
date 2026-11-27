module httptest

import net.http
import runtime
import time

// TestServer is a running HTTP server bound to 127.0.0.1 on an
// ephemeral port, for use in tests. It mirrors Go's httptest.Server.
pub struct TestServer {
pub mut:
	server &http.Server
pub:
	url string
}

// TestServerConfig configures a TestServer. `handler` is required and
// handles all requests. `worker_num` controls how many server workers
// accept connections; values <= 0 select a concurrent default of at least
// 2 workers, so that an idle keep-alive connection cannot starve the next
// client. Note that a single worker stays in the connection's read loop
// between HTTP/1.1 requests, so an explicit `worker_num` of 1 can still
// starve concurrent clients while a keep-alive connection is idle.
@[params]
pub struct TestServerConfig {
pub:
	handler        http.Handler @[required]
	worker_num     int
	accept_timeout time.Duration = 50 * time.millisecond
}

// new_server starts a test server with `handler` and waits until it
// accepts connections, returning the server and its base URL.
// It uses a concurrent worker default (at least 2 workers) so idle
// keep-alive clients never block other clients. Use
// `new_server_with_config` to tune the workers or timeouts before the
// server starts.
pub fn new_server(handler http.Handler) !TestServer {
	return new_server_with_config(handler: handler)!
}

// new_server_with_config starts a test server from `cfg` and waits until
// it accepts connections, returning the server and its base URL.
// A `cfg.worker_num` <= 0 selects a concurrent default of at least 2
// workers; pass an explicit positive `worker_num` to tune the pool
// before the server starts.
pub fn new_server_with_config(cfg TestServerConfig) !TestServer {
	mut worker_num := cfg.worker_num
	if worker_num <= 0 {
		worker_num = runtime.nr_jobs()
		if worker_num < 2 {
			worker_num = 2
		}
	}
	mut s := &http.Server{
		addr:                 '127.0.0.1:0'
		handler:              cfg.handler
		worker_num:           worker_num
		accept_timeout:       cfg.accept_timeout
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
