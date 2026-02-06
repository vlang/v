module picohttpparser

pub fn test_parses_a_simple_get_request() {
	mut req := Request{}
	parsed := req.parse_request('GET / HTTP/1.1\r\nHost: example.com\r\n\r\n') or {
		assert false, 'error while parse request: ${err}'
		0
	}

	assert parsed == 37
	assert req.method == 'GET'
	assert req.path == '/'
	assert req.headers[0].name == 'Host'
	assert req.headers[0].value == 'example.com'
}

pub fn test_parses_multiple_headers() {
	mut req := Request{}
	parsed := req.parse_request('GET /foo?bar=baz HTTP/1.1\r\nHeader1: value1\r\nHeader2: value2\r\n\r\n') or {
		assert false, 'error while parse request: ${err}'
		0
	}
	assert parsed == 63
	assert req.headers[1].name == 'Header2'
	assert req.headers[1].value == 'value2'
}

pub fn test_parses_requests_with_bodies() {
	mut req := Request{}
	parsed := req.parse_request('POST /data HTTP/1.1\r\nContent-Length: 10\r\n\r\nsomedata') or {
		assert false, 'error while parse request: ${err}'
		0
	}
	assert parsed == 43
	assert req.body == 'somedata'
}

pub fn test_handles_empty_requests() {
	mut req := Request{}
	parsed := req.parse_request('') or {
		assert false, 'error while parse request: ${err}'
		0
	}
	assert parsed == -2
}

pub fn test_handles_incomplete_requests() {
	mut req := Request{}
	partial_parsed := req.parse_request('GET /partial') or {
		assert false, 'error while parse request: ${err}'
		0
	}
	assert partial_parsed == -2
	assert req.prev_len == 0

	remaining_parsed := req.parse_request(' HTTP/1.1\r\n\r\n') or {
		assert err.msg() == 'error parsing request: invalid character "13"'
		0
	}
	assert remaining_parsed == 0
	assert req.method == ''
	assert req.path == ''
}
