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
