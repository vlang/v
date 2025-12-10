module fasthttp

/*
$if darwin {
	import fasthttp
	import time
	import os

	const text = 'hello world'

	// This is your custom application logic. The server will call this function
	// for each incoming request.
	fn request_handler(req fasthttp.HttpRequest) ![]u8 {
		s := req.buffer.bytestr()
		_ = s
		path := req.path.str()
		// println("REQUEST HANDLER() $path")
		// println('Handling request for path: "${path}"')

		// return []u8('<b>Hello from the IO thread!</b>')
		// return '<b>Hello from the IO thread!</b>'.bytes()

		match path {
			'/' {
				return text.bytes() //'<b>Hello from the IO thread!</b>'.bytes()
			}
			'/sleep' {
				// This code will run in a worker thread because the server
				// is configured to offload requests for this path.
				time.sleep(5 * time.second)
				return '<b>Hello from the worker thread after a 5s sleep!</b>'.bytes()
			}
			else {
				return '<b>404 Not Found</b>'.bytes()
			}
		}
	}

	fn test_lol() {
		// Create a new server instance on port 8092, passing our handler function.
		mut server := fasthttp.new_server(fasthttp.ServerConfig{
			port:    8092
			handler: request_handler
		}) or {
			eprintln('Failed to create server: ${err}')
			return
		}

		// Start the server's event loop. This function will block indefinitely.
		server.run() or { eprintln('Server failed to run: ${err}') }
	}
}
*/

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
