// vtest build: macos || freebsd || openbsd || netbsd || dragonfly
module fasthttp

import net
import net.http
import time

const host_bind_port = 13037

// Regression test for vlang/v issue #28119: a server configured with
// host: '127.0.0.1' must bind loopback only, even when family stays at the
// default .ip6. Previously host was ignored and the socket bound the wildcard
// address, leaving an "only local" service reachable on every interface.
fn test_host_binds_loopback_only() {
	mut server := new_server(ServerConfig{
		family:                  .ip6 // deliberately the default; host must still win
		host:                    '127.0.0.1'
		port:                    host_bind_port
		timeout_in_seconds:      2
		max_request_buffer_size: 8192
		handler:                 host_bind_handler
	}) or {
		assert false, 'Failed to create server: ${err}'
		return
	}
	handle := server.handle()
	spawn server.run()
	handle.wait_till_running(max_retries: 1000, retry_period_ms: 10) or {
		assert false, 'server did not start: ${err}'
		return
	}
	defer {
		handle.shutdown(timeout: 5 * time.second) or {}
	}

	// The actual bound address of the listening socket must be loopback IPv4,
	// not the 0.0.0.0 / :: wildcard.
	bound := net.addr_from_socket_handle(server.socket_fd)
	assert bound.family() == .ip
	assert bound.str() == '127.0.0.1:${host_bind_port}'

	// And it must still serve requests on that address.
	resp := http.fetch(
		method:                   .get
		url:                      'http://127.0.0.1:${host_bind_port}/'
		read_timeout:             2 * time.second
		write_timeout:            2 * time.second
		disable_connection_reuse: true
	) or {
		assert false, 'loopback request failed: ${err}'
		return
	}
	assert resp.status_code == 200
	assert resp.body == 'ok'
}

fn host_bind_handler(req HttpRequest) !HttpResponse {
	return HttpResponse{
		content: 'HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok'.bytes()
	}
}
