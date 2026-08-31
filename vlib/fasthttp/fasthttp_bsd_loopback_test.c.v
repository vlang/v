// vtest build: macos || freebsd || openbsd || netbsd || dragonfly
module fasthttp

import net.http
import time

const loopback_request_port = 13020
const loopback_request_addr = '127.0.0.1:${loopback_request_port}'

fn test_handler_can_make_loopback_request_to_same_server() {
	mut server := new_server(ServerConfig{
		family:                  .ip
		port:                    loopback_request_port
		timeout_in_seconds:      2
		max_request_buffer_size: 8192
		handler:                 loopback_request_handler
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

	resp := http.fetch(
		method:                   .get
		url:                      'http://${loopback_request_addr}/outer'
		read_timeout:             2 * time.second
		write_timeout:            2 * time.second
		disable_connection_reuse: true
	) or {
		assert false, 'loopback request failed: ${err}'
		return
	}
	assert resp.status_code == 200
	assert resp.body == 'outer:inner'
}

fn loopback_request_handler(req HttpRequest) !HttpResponse {
	path := req.buffer[req.path.start..req.path.start + req.path.len].bytestr()
	if path == '/inner' {
		return HttpResponse{
			content: 'HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\ninner'.bytes()
		}
	}
	if path == '/outer' {
		inner := http.fetch(
			method:                   .get
			url:                      'http://${loopback_request_addr}/inner'
			read_timeout:             time.second
			write_timeout:            time.second
			disable_connection_reuse: true
		)!
		body := 'outer:${inner.body}'
		return HttpResponse{
			content: 'HTTP/1.1 200 OK\r\nContent-Length: ${body.len}\r\n\r\n${body}'.bytes()
		}
	}
	return HttpResponse{
		content:      'HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()
		should_close: true
	}
}
