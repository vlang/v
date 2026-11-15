module http

fn test_dump_request_without_body() {
	req := new_request(.get, 'http://example.com/foo', '')
	d := dump_request(req, false)
	assert d.contains('GET ')
	assert d.contains('HTTP/1.1')
}

fn test_dump_request_with_body() {
	req := new_request(.post, 'http://example.com/foo', 'hello')
	d := dump_request(req, true)
	assert d.contains('hello')
}

fn test_dump_response_without_body() {
	mut resp := Response{ status_code: 200, status_msg: 'OK', http_version: '1.1' }
	resp.header.add(.content_type, 'text/plain')
	d := dump_response(resp, false)
	assert d.contains('200 OK')
	assert !d.contains('hello')
}

fn test_dump_response_with_body() {
	mut resp := Response{ status_code: 200, status_msg: 'OK', http_version: '1.1', body: 'hello' }
	resp.header.add(.content_type, 'text/plain')
	d := dump_response(resp, true)
	assert d.contains('hello')
}

fn test_dump_request_preserves_exact_body_bytes() {
	for data in ['', 'hello', 'hello\n', 'hello\r\n', [u8(0), 0xff, 13, 10, 1].bytestr()] {
		req := Request{
			method: .post
			url:    '/upload?q=1'
			data:   data
		}
		prefix := 'POST /upload?q=1 HTTP/1.1\r\n\r\n'
		assert dump_request(req, true) == prefix + data
		assert dump_request(req, false) == prefix
		assert req.data == data
	}
}

fn test_dump_request_uses_stored_url_and_default_target() {
	assert dump_request(Request{}, false) == 'GET / HTTP/1.1\r\n\r\n'
	req := Request{
		url:     'https://example.com/path?q=1'
		version: .v1_0
	}
	assert dump_request(req, false) == 'GET https://example.com/path?q=1 HTTP/1.0\r\n\r\n'
}

fn test_dump_request_headers_follow_selected_version() {
	mut req := Request{
		url:     '/'
		version: .v2_0
	}
	req.header.add_custom('X-Value', ' \tfirst\t ') or { panic(err) }
	req.header.add_custom('X-Value', 'second') or { panic(err) }
	assert dump_request(req, false) == 'GET / HTTP/2.0\r\nx-value: first\r\nx-value: second\r\n\r\n'
	assert req.header.get_custom('X-Value') or { '' } == ' \tfirst\t '
}

fn test_dump_response_body_switch_preserves_header_prefix() {
	for version in ['1.0', '1.1', '2.0'] {
		for reason in ['', 'Custom status'] {
			mut resp := Response{
				http_version: version
				status_code:  299
				status_msg:   reason
				body:         'hello\x00\xff'
			}
			resp.header.add(.content_length, resp.body.len.str())
			resp.header.add_custom('X-Value', ' \tfirst\t ') or { panic(err) }
			resp.header.add_custom('X-Value', 'second') or { panic(err) }
			with_body := dump_response(resp, true)
			without_body := dump_response(resp, false)
			assert with_body == resp.bytestr()
			assert without_body + resp.body == with_body
			assert without_body.starts_with('HTTP/${version} 299 ${reason}\r\n')
			assert without_body.ends_with('\r\n\r\n')
			assert resp.body == 'hello\x00\xff'
			assert resp.header.get_custom('X-Value') or { '' } == ' \tfirst\t '
		}
	}
}

fn test_dump_response_exact_output() {
	resp := Response{
		http_version: '1.1'
		status_code:  200
		status_msg:   'OK'
		header:       new_header(key: .content_length, value: '5')
		body:         'hello'
	}
	prefix := 'HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\n'
	assert dump_response(resp, false) == prefix
	assert dump_response(resp, true) == prefix + 'hello'
}
