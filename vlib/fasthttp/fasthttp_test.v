module fasthttp

import os

const fasthttp_example_exe = os.join_path(os.cache_dir(), 'fasthttp_example_test.exe')

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
	req := parse_request_line(request) or {
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
	req := parse_request_line(request) or {
		assert false, 'Failed to parse valid request: ${err}'
		return
	}

	path := req.buffer[req.path.start..req.path.start + req.path.len].bytestr()
	assert path == '/users/123'
}

fn test_parse_request_line_post() {
	// Test POST request
	request := 'POST /api/data HTTP/1.1\r\n'.bytes()
	req := parse_request_line(request) or {
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
	parse_request_line(request) or {
		assert err.msg() == 'Invalid HTTP request line'
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
	handler := fn (req HttpRequest) ![]u8 {
		return 'HTTP/1.1 200 OK\r\n\r\nHello'.bytes()
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
