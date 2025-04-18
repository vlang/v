module request_parser

fn test_parse_http1_request_line_valid_request() {
	buffer := 'GET /path/to/resource HTTP/1.1\r\n'.bytes()
	mut req := HttpRequest{
		buffer: buffer
	}

	parse_http1_request_line(mut req) or { panic(err) }

	assert slice_to_string(req.buffer, req.method) == 'GET'
	assert slice_to_string(req.buffer, req.path) == '/path/to/resource'
	assert slice_to_string(req.buffer, req.version) == 'HTTP/1.1'
}

fn test_parse_http1_request_line_invalid_request() {
	buffer := 'INVALID REQUEST LINE'.bytes()
	mut req := HttpRequest{
		buffer: buffer
	}

	mut has_error := false
	parse_http1_request_line(mut req) or {
		has_error = true
		assert err.msg() == 'Invalid HTTP request line'
	}
	assert has_error, 'Expected error for invalid request line'
}

fn test_decode_http_request_valid_request() {
	buffer := 'POST /api/resource HTTP/1.0\r\n'.bytes()
	req := decode_http_request(buffer) or { panic(err) }

	assert slice_to_string(req.buffer, req.method) == 'POST'
	assert slice_to_string(req.buffer, req.path) == '/api/resource'
	assert slice_to_string(req.buffer, req.version) == 'HTTP/1.0'
}

fn test_decode_http_request_invalid_request() {
	buffer := 'INVALID REQUEST LINE'.bytes()

	mut has_error := false
	decode_http_request(buffer) or {
		has_error = true
		assert err.msg() == 'Invalid HTTP request line'
	}
	assert has_error, 'Expected error for invalid request'
}

fn test_get_header_value_slice_existing_header() {
	buffer := 'GET / HTTP/1.1\r\nHost: example.com\r\nContent-Type: text/html\r\n\r\n'.bytes()
	req := decode_http_request(buffer) or { panic(err) }

	host_slice := req.get_header_value_slice('Host') or { panic('Header not found') }
	assert slice_to_string(req.buffer, host_slice) == 'example.com'

	content_type_slice := req.get_header_value_slice('Content-Type') or {
		panic('Header not found')
	}
	assert slice_to_string(req.buffer, content_type_slice) == 'text/html'
}

fn test_get_header_value_slice_non_existing_header() {
	buffer := 'GET / HTTP/1.1\r\nHost: example.com\r\n\r\n'.bytes()
	req := decode_http_request(buffer) or { panic(err) }

	assert req.get_header_value_slice('Content-Type') == none
}
