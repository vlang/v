module http

fn test_response_bytestr_1() {
	resp := new_response(
		status: .ok
		body:   'Foo'
	)
	assert resp.bytestr() == 'HTTP/1.1 200 OK\r\n' + 'Content-Length: 3\r\n' + '\r\n' + 'Foo'
}

fn test_response_bytestr_2() {
	resp := new_response(
		status: .found
		body:   'Foo'
		header: new_header(key: .location, value: '/')
	)
	lines := resp.bytestr().split_into_lines()
	assert lines[0] == 'HTTP/1.1 302 Found'
	// header order is not guaranteed
	check_headers(['Location: /', 'Content-Length: 3'], lines[1..3])!
	assert lines[3] == ''
	assert lines[4] == 'Foo'
}

// check_headers is a helper function for asserting all expected headers
// are found because rendered header order is not guaranteed. The check
// is O(n^2) which is fine for small lists.
fn check_headers(expected []string, found []string) ! {
	assert expected.len == found.len
	for header in expected {
		if !found.contains(header) {
			return error('expected header "${header}" not in ${found}')
		}
	}
}

fn test_parse_response() {
	content := 'HTTP/1.1 200 OK\r\nContent-Length: 3\r\n\r\nFoo'
	x := parse_response(content)!
	assert x.http_version == '1.1'
	assert x.status_code == 200
	assert x.status_msg == 'OK'
	assert x.header.contains(.content_length)
	assert x.header.get(.content_length)! == '3'
	assert x.body == 'Foo'
}

fn test_parse_response_with_cookies() {
	cookie_id := 'v_is_best'
	content := 'HTTP/1.1 200 OK\r\nSet-Cookie: id=${cookie_id}\r\nContent-Length: 3\r\n\r\nFoo'
	mut x := parse_response(content)!
	assert x.http_version == '1.1'
	assert x.status_code == 200
	assert x.status_msg == 'OK'
	assert x.header.contains(.content_length)
	assert x.header.get(.content_length)! == '3'
	assert x.body == 'Foo'
	response_cookie := x.cookies()
	assert response_cookie[0].str() == 'id=${cookie_id}'

	// cookie has Base64 encoding info, ending with '=='
	cookie_base64 := 'Ln0kBnAaAyYFQ8lH7d5J8Y5w1/iyDRpj6d0nBLTbBUMbtEyPD32rPvpApsvxhLJWlkHuHT3KYL0g/xNBxC9od5tMFAgurLxKdRd5lZ6Pd7W+SllkbsXmUA=='
	content_cookie_base64 := 'HTTP/1.1 200 OK\r\nSet-Cookie: enctoken=${cookie_base64}; path=/; secure; SameSite=None\r\nContent-Length: 3\r\n\r\nFoo'
	x = parse_response(content_cookie_base64)!
	assert x.http_version == '1.1'
	assert x.status_code == 200
	assert x.status_msg == 'OK'
	assert x.header.contains(.content_length)
	assert x.header.get(.content_length)! == '3'
	assert x.body == 'Foo'
	response_cookie_base64 := x.cookies()
	assert response_cookie_base64[0].str().split(';')[0] == 'enctoken=${cookie_base64}'
}

fn test_parse_response_with_weird_cookie() {
	// weird cookies test
	content_weird := 'HTTP/1.1 200 OK\r\nSet-Cookie: a=b; ; =; aa=; =bb; cc; ==\r\nContent-Length: 3\r\n\r\nFoo'
	mut xx := parse_response(content_weird)!
	weird_cookie := xx.cookies()
	assert weird_cookie[0].str() == 'a=b'
}
