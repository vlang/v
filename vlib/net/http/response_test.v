module http

fn test_parse_response_line() ? {
	if _ := parse_response_line('hello world') {
		return error('should be error')
	}
	mut version, mut status := parse_response_line('http/1.1 200 OK') or {
		return error('should not error')
	}
	assert version == .v1_1
	assert status == .ok

	version, status = parse_response_line('http/1.1 200 any string') or {
		return error('should not error')
	}
	assert version == .v1_1
	assert status == .ok

	// TODO: should this not parse?
	version, status = parse_response_line('abc 123') or { return error('should not error') }
	assert version == .unknown
	assert status == .unassigned
}

fn test_response_render() ? {
	resp := Response{
		text: 'hello'
		header: new_header(key: .content_type, value: 'text/plain')
		cookies: map{
			'cookie': 'jar'
		}
		status_code: .ok
		http_version: .v1_1
	}
	assert resp.render() == 'HTTP/1.1 200 OK\n\rContent-Type: text/plain\n\rContent-Length: 5\n\rSet-Cookie: cookie=jar\n\r\n\rhello'
}
