module fasthttp

import net
import os
import time

const fasthttp_example_exe = os.join_path(os.cache_dir(), 'fasthttp_example_test.exe')
const reusable_takeover_port = 13019
const reusable_takeover_addr = '127.0.0.1:${reusable_takeover_port}'

fn testsuite_begin() {
	// Clean up old example binary if it exists
	if os.exists(fasthttp_example_exe) {
		os.rm(fasthttp_example_exe) or {}
	}
}

fn test_fasthttp_example_compiles() {
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)

	// Build the fasthttp example
	build_result := os.system('${os.quoted_path(vexe)} -o ${os.quoted_path(fasthttp_example_exe)} ${os.join_path(vroot,
		'examples', 'fasthttp')}')
	assert build_result == 0, 'fasthttp example failed to compile'
	assert os.exists(fasthttp_example_exe), 'fasthttp example binary not found after build'
}

fn test_parse_request_line() {
	// Test basic GET request
	request := 'GET / HTTP/1.1\r\n'.bytes()
	req := decode_http_request(request) or {
		assert false, 'Failed to parse valid request: ${err}'
		return
	}

	assert req.buffer.len == request.len
	assert req.method.start == 0
	assert req.method.len == 3
	assert req.path.start == 4
	assert req.path.len == 1
	assert req.version.start == 6
	assert req.version.len == 8

	method := req.buffer[req.method.start..req.method.start + req.method.len].bytestr()
	path := req.buffer[req.path.start..req.path.start + req.path.len].bytestr()
	version := req.buffer[req.version.start..req.version.start + req.version.len].bytestr()

	assert method == 'GET'
	assert path == '/'
	assert version == 'HTTP/1.1'
}

fn test_parse_request_line_with_path() {
	// Test GET request with path
	request := 'GET /users/123 HTTP/1.1\r\n'.bytes()
	req := decode_http_request(request) or {
		assert false, 'Failed to parse valid request: ${err}'
		return
	}

	path := req.buffer[req.path.start..req.path.start + req.path.len].bytestr()
	assert path == '/users/123'
}

fn test_parse_request_line_post() {
	// Test POST request
	request := 'POST /api/data HTTP/1.1\r\n'.bytes()
	req := decode_http_request(request) or {
		assert false, 'Failed to parse valid request: ${err}'
		return
	}

	method := req.buffer[req.method.start..req.method.start + req.method.len].bytestr()
	path := req.buffer[req.path.start..req.path.start + req.path.len].bytestr()

	assert method == 'POST'
	assert path == '/api/data'
}

fn test_parse_request_line_invalid() {
	// Test invalid request (missing \r\n)
	request := 'GET / HTTP/1.1'.bytes()
	decode_http_request(request) or {
		assert err.msg() == 'Invalid HTTP request line: Missing CR'
		return
	}
	assert false, 'Should have failed to parse invalid request'
}

fn test_decode_http_request() {
	request := 'GET /test HTTP/1.1\r\n'.bytes()
	req := decode_http_request(request) or {
		assert false, 'Failed to decode request: ${err}'
		return
	}

	method := req.buffer[req.method.start..req.method.start + req.method.len].bytestr()
	assert method == 'GET'
}

fn test_new_server() {
	handler := fn (req HttpRequest) !HttpResponse {
		return HttpResponse{
			content: 'HTTP/1.1 200 OK\r\n\r\nHello'.bytes()
		}
	}

	server := new_server(ServerConfig{
		port:    8080
		handler: handler
	}) or {
		assert false, 'Failed to create server: ${err}'
		return
	}

	assert server.port == 8080
}

fn test_server_ipv4_ipv6_binding() {
	// Test IPv4 binding
	handler := fn (req HttpRequest) !HttpResponse {
		return HttpResponse{
			content: 'HTTP/1.1 200 OK\r\n\r\nIPv4 test'.bytes()
		}
	}

	server_ipv4 := new_server(ServerConfig{
		family:  .ip
		port:    8081
		handler: handler
	}) or {
		assert false, 'Failed to create IPv4 server: ${err}'
		return
	}

	// Test IPv6 binding
	server_ipv6 := new_server(ServerConfig{
		family:  .ip6
		port:    8082
		handler: handler
	}) or {
		assert false, 'Failed to create IPv6 server: ${err}'
		return
	}

	// Verify both servers were created successfully
	// Note: family field is not exported, so we can't directly test it
	assert server_ipv4.port == 8081
	assert server_ipv6.port == 8082
}

fn test_response_takeover_mode_reusable_keeps_connection() {
	$if linux || bsd {
		mut server := new_server(ServerConfig{
			family:                  .ip
			port:                    reusable_takeover_port
			timeout_in_seconds:      2
			max_request_buffer_size: 8192
			handler:                 reusable_takeover_handler
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

		mut conn := net.dial_tcp(reusable_takeover_addr)!
		conn.set_read_timeout(2 * time.second)
		conn.set_write_timeout(2 * time.second)
		defer {
			conn.close() or {}
		}

		conn.write_string('GET /reus')!
		time.sleep(50 * time.millisecond)
		conn.write_string('able HTTP/1.1\r\nHost: ${reusable_takeover_addr}\r\n\r\n')!
		reusable_response := read_until_contains(mut conn, '\r\n0\r\n\r\n')!
		assert reusable_response.contains('manual') == true, reusable_response
		assert reusable_response.contains('\r\n0\r\n\r\n') == true, reusable_response

		conn.write_string('GET /normal HTTP/1.1\r\nHost: ${reusable_takeover_addr}\r\n\r\n')!
		normal_response := read_until_contains(mut conn, 'normal')!
		assert normal_response.contains('normal') == true, normal_response
		assert normal_response.contains('Connection: close') == false, normal_response
	} $else {
		return
	}
}

fn reusable_takeover_handler(req HttpRequest) !HttpResponse {
	path := req.buffer[req.path.start..req.path.start + req.path.len].bytestr()
	if path == '/reusable' {
		body := 'manual'
		send_raw_response(req.client_conn_fd,
			'HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n${body.len:x}\r\n${body}\r\n0\r\n\r\n')
		return HttpResponse{
			takeover_mode: .reusable
		}
	}
	return HttpResponse{
		content: 'HTTP/1.1 200 OK\r\nContent-Length: 6\r\n\r\nnormal'.bytes()
	}
}

fn send_raw_response(fd int, response string) {
	$if linux {
		C.send(fd, response.str, response.len, C.MSG_NOSIGNAL)
	} $else $if bsd {
		C.send(fd, response.str, response.len, send_flags)
	} $else {
		C.send(fd, response.str, response.len, 0)
	}
}

fn read_until_contains(mut conn net.TcpConn, marker string) !string {
	mut raw := ''
	mut buf := []u8{len: 1024}
	for _ in 0 .. 16 {
		n := conn.read(mut buf)!
		if n <= 0 {
			break
		}
		raw += buf[..n].bytestr()
		if raw.contains(marker) {
			break
		}
	}
	return raw
}
