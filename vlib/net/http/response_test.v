module http

import compress.gzip
import compress.zlib

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

fn test_parse_response_with_gzip_content_encoding() {
	expected_body := '{"a": 1}'
	compressed_body := gzip.compress(expected_body.bytes())!
	content :=
		'HTTP/1.1 200 OK\r\nContent-Encoding: gzip\r\nContent-Length: ${compressed_body.len}\r\n\r\n' +
		compressed_body.bytestr()
	resp := parse_response(content)!
	assert resp.body == expected_body
}

fn test_parse_response_with_deflate_content_encoding() {
	expected_body := '{"a": 1}'
	compressed_body := zlib.compress(expected_body.bytes())!
	content :=
		'HTTP/1.1 200 OK\r\nContent-Encoding: deflate\r\nContent-Length: ${compressed_body.len}\r\n\r\n' +
		compressed_body.bytestr()
	resp := parse_response(content)!
	assert resp.body == expected_body
}

fn test_parse_response_with_chunked_and_gzip_content_encoding() {
	expected_body := '{"a": 1}'
	compressed_body := gzip.compress(expected_body.bytes())!
	chunked_body := '${compressed_body.len:x}\r\n' + compressed_body.bytestr() + '\r\n0\r\n\r\n'
	content := 'HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nContent-Encoding: gzip\r\n\r\n' +
		chunked_body
	resp := parse_response(content)!
	assert resp.body == expected_body
}

fn test_vschannel_error_message_formats_non_wsa_errors() {
	assert vschannel_error_message(-2146893052) == '0x80090304'
	assert vschannel_error_message(42) == '42'
}

fn test_vschannel_error_message_formats_windows_wsa_errors() {
	$if windows {
		assert vschannel_error_message(11001) == '(11001) wsahost_not_found'
	}
}

fn test_vschannel_request_error_normalizes_connect_failures() {
	err := vschannel_request_error(vschannel_sec_e_internal_error)
	assert err.msg() == vschannel_connect_failed_msg
	assert err.code() == vschannel_sec_e_internal_error
}

fn test_vschannel_request_error_keeps_other_codes() {
	err := vschannel_request_error(42)
	assert err.msg() == 'http: vschannel request failed: 42'
	assert err.code() == 42
}

fn test_vschannel_parse_response_normalizes_connect_failure_output() {
	vschannel_parse_response('Error 10057 sending data to server (1)\nError performing handshake',
		0) or {
		assert err.msg() == vschannel_connect_failed_msg
		assert err.code() == 0
		return
	}
	assert false
}
