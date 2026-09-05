// vtest build: macos || freebsd || openbsd || netbsd || dragonfly
module fasthttp

import net.http
import time
import net

fn C.shutdown(fd i32, how i32) i32

const loopback_request_port = 13020
const loopback_request_addr = '127.0.0.1:${loopback_request_port}'

fn test_handler_can_make_loopback_request_to_same_server() {
	mut server := new_server(ServerConfig{
		family: .ip
		port: loopback_request_port
		timeout_in_seconds: 2
		max_request_buffer_size: 8192
		handler: loopback_request_handler
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
		method: .get
		url: 'http://${loopback_request_addr}/outer'
		read_timeout: 2 * time.second
		write_timeout: 2 * time.second
		disable_connection_reuse: true
	) or {
		assert false, 'loopback request failed: ${err}'
		return
	}
	assert resp.status_code == 200
	assert resp.body == 'outer:inner'

	mut conn := net.dial_tcp(loopback_request_addr) or {
		assert false, 'pipelining connection failed: ${err}'
		return
	}
	defer {
		conn.close() or {}
	}
	conn.set_read_timeout(2 * time.second)
	conn.set_write_timeout(2 * time.second)
	conn.write_string('GET /one HTTP/1.1\r\nHost: ${loopback_request_addr}\r\n\r\n' + 'GET /two HTTP/1.1\r\nHost: ${loopback_request_addr}\r\nConnection: close\r\n\r\n') or {
		assert false, 'pipelined write failed: ${err}'
		return
	}
	assert C.shutdown(conn.sock.handle, C.SHUT_WR) == 0
	mut pipelined := []u8{}
	mut chunk := []u8{len: 1024}
	for pipelined.bytestr().count('HTTP/1.1 200 OK') < 2 {
		n := conn.read(mut chunk) or {
			assert false, 'pipelined read failed: ${err}'
			return
		}
		if n == 0 {
			break
		}
		pipelined << chunk[..n]
	}
	pipelined_response := pipelined.bytestr()
	assert pipelined_response.count('HTTP/1.1 200 OK') == 2, pipelined_response
	one_pos := pipelined_response.index('one') or { -1 }
	two_pos := pipelined_response.index('two') or { -1 }
	assert one_pos >= 0 && two_pos > one_pos
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
			method: .get
			url: 'http://${loopback_request_addr}/inner'
			read_timeout: time.second
			write_timeout: time.second
			disable_connection_reuse: true
		)!
		body := 'outer:${inner.body}'
		return HttpResponse{
			content: 'HTTP/1.1 200 OK\r\nContent-Length: ${body.len}\r\n\r\n${body}'.bytes()
		}
	}
	if path == '/one' || path == '/two' {
		body := path[1..]
		return HttpResponse{
			content: 'HTTP/1.1 200 OK\r\nContent-Length: ${body.len}\r\n\r\n${body}'.bytes()
			should_close: path == '/two'
		}
	}
	return HttpResponse{
		content: 'HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()
		should_close: true
	}
}
