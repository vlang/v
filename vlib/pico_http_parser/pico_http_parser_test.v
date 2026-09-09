module pico_http_parser

pub fn test_request_alias_parses_a_simple_get_request() {
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

pub fn test_u64toa_alias_formats_numbers() {
	mut buf := [10]u8{}
	len := unsafe {
		u64toa(&buf[0], 12345) or {
			assert false, 'error while formatting number: ${err}'
			0
		}
	}

	assert len == 5
	assert buf[0..len] == '12345'.bytes()
}

pub fn test_response_alias_can_be_initialized() {
	response := Response{}

	assert response.fd == 0
}
