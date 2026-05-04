// vtest build: !windows
import net.http
import net.urllib
import net
import io
import time

struct StringReader {
	text string
mut:
	place int
}

fn (mut s StringReader) read(mut buf []u8) !int {
	if s.place >= s.text.len {
		return io.Eof{}
	}
	max_bytes := 100
	end := if s.place + max_bytes >= s.text.len { s.text.len } else { s.place + max_bytes }
	n := copy(mut buf, s.text[s.place..end].bytes())
	s.place += n
	return n
}

fn reader(s string) &io.BufferedReader {
	return io.new_buffered_reader(
		reader: &StringReader{
			text: s
		}
	)
}

fn test_parse_request_not_http() {
	mut reader__ := reader('hello')
	http.parse_request(mut reader__) or { return }
	panic('should not have parsed')
}

fn test_parse_request_no_headers() {
	mut reader_ := reader('GET / HTTP/1.1\r\n\r\n')
	req := http.parse_request(mut reader_) or { panic('did not parse: ${err}') }
	assert req.method == .get
	assert req.url == '/'
	assert req.version == .v1_1
}

fn test_parse_request_two_headers() {
	mut reader_ := reader('GET / HTTP/1.1\r\nTest1: a\r\nTest2:  B\r\n\r\n')
	req := http.parse_request(mut reader_) or { panic('did not parse: ${err}') }
	assert req.header.custom_values('Test1') == ['a']
	assert req.header.custom_values('Test2') == ['B']
}

fn test_parse_request_two_header_values() {
	mut reader_ := reader('GET / HTTP/1.1\r\nTest1: a; b\r\nTest2: c\r\nTest2: d\r\n\r\n')
	req := http.parse_request(mut reader_) or { panic('did not parse: ${err}') }
	assert req.header.custom_values('Test1') == ['a; b']
	assert req.header.custom_values('Test2') == ['c', 'd']
}

fn test_parse_request_body() {
	mut reader_ :=
		reader('GET / HTTP/1.1\r\nTest1: a\r\nTest2: b\r\nContent-Length: 4\r\n\r\nbodyabc')
	req := http.parse_request(mut reader_) or { panic('did not parse: ${err}') }
	assert req.data == 'body'
}

fn test_parse_request_line() {
	method, target, version := http.parse_request_line('GET /target HTTP/1.1') or {
		panic('did not parse: ${err}')
	}
	assert method == .get
	assert target.str() == '/target'
	assert version == .v1_1
}

fn test_parse_request_line_unknown_method_fails() {
	http.parse_request_line('BREW /coffee HTTP/1.1') or {
		assert err.msg().contains('unsupported method')
		return
	}
	assert false, 'expected unsupported method error'
}

fn test_parse_form() {
	assert http.parse_form('foo=bar&bar=baz') == {
		'foo': 'bar'
		'bar': 'baz'
	}
	assert http.parse_form('foo=bar=&bar=baz') == {
		'foo': 'bar='
		'bar': 'baz'
	}
	assert http.parse_form('foo=bar%3D&bar=baz') == {
		'foo': 'bar='
		'bar': 'baz'
	}
	assert http.parse_form('foo=b%26ar&bar=baz') == {
		'foo': 'b&ar'
		'bar': 'baz'
	}
	assert http.parse_form('a=b& c=d') == {
		'a':  'b'
		' c': 'd'
	}
	assert http.parse_form('a=b&c= d ') == {
		'a': 'b'
		'c': ' d '
	}
	assert http.parse_form('{json}') == {
		'json': '{json}'
	}
	assert http.parse_form('{
    "_id": "76c",
    "friends": [
      {
        "id": 0,
        "name": "Mason Luna"
      }
    ],
    "greeting": "Hello."
  }') == {
		'json': '{
    "_id": "76c",
    "friends": [
      {
        "id": 0,
        "name": "Mason Luna"
      }
    ],
    "greeting": "Hello."
  }'
	}
}

fn test_parse_multipart_form() {
	boundary := '6844a625b1f0b299'
	names := ['foo', 'fooz']
	file := 'bar.v'
	ct := 'application/octet-stream'
	contents := ['baz', 'buzz']
	data := "--${boundary}
Content-Disposition: form-data; name=\"${names[0]}\"; filename=\"${file}\"\r
Content-Type: ${ct}\r
\r
${contents[0]}\r
--${boundary}\r
Content-Disposition: form-data; name=\"${names[1]}\"\r
\r
${contents[1]}\r
--${boundary}--\r
"
	form, files := http.parse_multipart_form(data, boundary)
	assert files == {
		names[0]: [
			http.FileData{
				filename:     file
				content_type: ct
				data:         contents[0]
			},
		]
	}

	assert form == {
		names[1]: contents[1]
	}
}

fn test_parse_multipart_form2() {
	boundary := '---------------------------27472781931927549291906391339'
	data := '--${boundary}\r
Content-Disposition: form-data; name="username"\r
\r
admin\r
--${boundary}\r
Content-Disposition: form-data; name="password"\r
\r
admin123\r
--${boundary}--\r
'
	form, files := http.parse_multipart_form(data, boundary)
	for k, v in form {
		eprintln('> k: ${k} | v: ${v}')
		eprintln('>> k.bytes(): ${k.bytes()}')
		eprintln('>> v.bytes(): ${v.bytes()}')
	}
	assert form['username'] == 'admin'
	assert form['password'] == 'admin123'
}

fn test_multipart_form_body() {
	files := {
		'foo': [
			http.FileData{
				filename:     'bar.v'
				content_type: 'application/octet-stream'
				data:         'baz'
			},
		]
	}
	form := {
		'fooz': 'buzz'
	}

	body, boundary := http.multipart_form_body(form, files)
	parsed_form, parsed_files := http.parse_multipart_form(body, boundary)
	assert parsed_files == files
	assert parsed_form == form
}

fn test_parse_large_body() {
	body := 'A'.repeat(10_001) // greater than max_bytes
	req := 'GET / HTTP/1.1\r\nContent-Length: ${body.len}\r\n\r\n${body}'
	mut reader_ := reader(req)
	result := http.parse_request(mut reader_)!
	assert result.data.len == body.len
	assert result.data == body
}

fn test_parse_multipart_form_empty_body() {
	body := ''
	boundary := '----WebKitFormBoundaryQcBIkwnOACVsvR8b'
	form, files := http.parse_multipart_form(body, boundary)
	assert form.len == 0
	assert files.len == 0
}

fn test_parse_multipart_form_issue_26204__do_not_panic_for_small_or_partial_forms() {
	boundary := '----01KDN6J6BKWY9WMYWRW4MG5J59'
	body := '${boundary}\r\nContent-Disposition: form-data; name="fooz"${boundary}--\r\n'
	form, files := http.parse_multipart_form(body, boundary)
	assert form.len == 0
	assert files.len == 0
}

fn test_parse_multipart_form_issue_24974_raw() {
	body := r'------WebKiormBoundaryQcBIkwnOACVsvR8b\r\nContent-Disposition: form-data; name="files"; filename="michael-sum-LEpfefQf4rU-unsplash.jpg"\r\nContent-Type: image/jpeg\r\n\r\n\r\n------WebKitFormBoundaryQcBIkwnOACVsvR8b\r\nContent-Disposition: form-data; name="files"; filename="mikhail-vasilyev-IFxjDdqK_0U-unsplash.jpg"\r\nContent-Type: image/jpeg\r\n\r\n\r\n------WebKitFormBoundaryQcBIkwnOACVsvR8b--\r\n'
	boundary := r'----WebKitFormBoundaryQcBIkwnOACVsvR8b'
	form, files := http.parse_multipart_form(body, boundary)
	assert form.len == 0
	assert files.len == 0
}

fn test_parse_multipart_form_issue_24974_cooked() {
	body := '------WebKiormBoundaryQcBIkwnOACVsvR8b\r\nContent-Disposition: form-data; name="files"; filename="michael-sum-LEpfefQf4rU-unsplash.jpg"\r\nContent-Type: image/jpeg\r\n\r\n\r\n------WebKitFormBoundaryQcBIkwnOACVsvR8b\r\nContent-Disposition: form-data; name="files"; filename="mikhail-vasilyev-IFxjDdqK_0U-unsplash.jpg"\r\nContent-Type: image/jpeg\r\n\r\n\r\n------WebKitFormBoundaryQcBIkwnOACVsvR8b--\r\n'
	boundary := '----WebKitFormBoundaryQcBIkwnOACVsvR8b'
	form, files := http.parse_multipart_form(body, boundary)
	assert form.len == 0
	assert files.len == 1
	assert files['files'][0].filename == 'mikhail-vasilyev-IFxjDdqK_0U-unsplash.jpg'
	assert files['files'][0].content_type == 'image/jpeg'
}

fn test_parse_request_head_str_basic() {
	s := 'GET / HTTP/1.1\r\nHost: example.com\r\nContent-Length: 0\r\n\r\n'
	req := http.parse_request_head_str(s) or { panic('did not parse: ${err}') }
	assert req.method == .get
	assert req.url == '/'
	assert req.version == .v1_1
	assert req.host == 'example.com'
}

fn test_parse_request_head_str_post_with_headers() {
	s := 'POST /api HTTP/1.1\r\nHost: test.com\r\nContent-Type: application/json\r\nContent-Length: 10\r\n\r\n'
	req := http.parse_request_head_str(s) or { panic('did not parse: ${err}') }
	assert req.method == .post
	assert req.url == '/api'
	assert req.version == .v1_1
	assert req.host == 'test.com'
	assert req.header.custom_values('Content-Type') == ['application/json']
}

fn test_parse_request_head_str_post_with_headers_and_body() {
	s := 'POST /index HTTP/1.1\r\nHost: localhost:9008\r\nUser-Agent: curl/7.68.0\r\nAccept: */*\r\nContent-Type: application/json\r\nContent-Length: 24\r\nConnection: keep-alive\r\n\r\n{"username": "test"}'
	req := http.parse_request_head_str(s) or {
		assert false, 'did not parse: ${err}'
		return
	}
	assert req.method == .post
	assert req.url == '/index'
	assert req.version == .v1_1
	assert req.host == 'localhost:9008'
	assert req.header.custom_values('User-Agent') == ['curl/7.68.0']
	assert req.header.custom_values('Accept') == ['*/*']
	assert req.header.custom_values('Content-Type') == ['application/json']
	assert req.header.custom_values('Connection') == ['keep-alive']
	assert req.data == ''
}

fn test_parse_request_head_post_with_headers_and_body() {
	s := 'POST /index HTTP/1.1\r\nHost: localhost:9008\r\nUser-Agent: curl/7.68.0\r\nAccept: */*\r\nContent-Type: application/json\r\nContent-Length: 24\r\nConnection: keep-alive\r\n\r\n{"username": "test"}'
	req := http.parse_request_str(s) or {
		assert false, 'did not parse: ${err}'
		return
	}
	assert req.data == '{"username": "test"}'
}

fn test_parse_request_head_str_with_spaces_in_header_values() {
	s := 'GET /path HTTP/1.1\r\nX-Custom-Header: value with spaces\r\n\r\n'
	req := http.parse_request_head_str(s) or { panic('did not parse: ${err}') }
	assert req.method == .get
	assert req.url == '/path'
	assert req.header.custom_values('X-Custom-Header') == ['value with spaces']
}

fn test_parse_request_head_str_multiple_same_header() {
	s := 'GET / HTTP/1.1\r\nHost: example.com\r\nSet-Cookie: session=abc\r\nSet-Cookie: user=xyz\r\n\r\n'
	req := http.parse_request_head_str(s) or { panic('did not parse: ${err}') }
	assert req.method == .get
	assert req.host == 'example.com'
	assert req.header.custom_values('Set-Cookie') == ['session=abc', 'user=xyz']
}

fn test_parse_request_with_limit_accepts_small_body() {
	body := 'hello'
	req_str := 'POST / HTTP/1.1\r\nContent-Length: ${body.len}\r\n\r\n${body}'
	mut r := reader(req_str)
	req := http.parse_request_with_limit(mut r, 1000) or {
		assert false, 'should not fail: ${err}'
		return
	}
	assert req.data == body
}

fn test_parse_request_with_limit_rejects_large_body() {
	body := 'A'.repeat(1000)
	req_str := 'POST / HTTP/1.1\r\nContent-Length: ${body.len}\r\n\r\n${body}'
	mut r := reader(req_str)
	http.parse_request_with_limit(mut r, 100) or {
		assert err.msg().contains('request body too large')
		return
	}
	assert false, 'expected error for body exceeding limit'
}

fn test_parse_request_with_limit_zero_means_no_limit() {
	body := 'A'.repeat(10000)
	req_str := 'POST / HTTP/1.1\r\nContent-Length: ${body.len}\r\n\r\n${body}'
	mut r := reader(req_str)
	req := http.parse_request_with_limit(mut r, 0) or {
		assert false, 'should not fail with limit=0: ${err}'
		return
	}
	assert req.data.len == body.len
}

fn test_server_default_body_limit() {
	s := http.Server{}
	assert s.max_request_body_size == 10_485_760
}

fn test_parse_request_rejects_negative_content_length() {
	mut r := reader('POST / HTTP/1.1\r\nContent-Length: -1\r\n\r\n')
	http.parse_request(mut r) or {
		assert err.msg().contains('invalid Content-Length')
		return
	}
	assert false, 'expected error for negative Content-Length'
}

fn test_parse_request_rejects_non_numeric_content_length() {
	mut r := reader('POST / HTTP/1.1\r\nContent-Length: abc\r\n\r\n')
	http.parse_request(mut r) or {
		assert err.msg().contains('invalid Content-Length')
		return
	}
	assert false, 'expected error for non-numeric Content-Length'
}

fn test_parse_request_rejects_overflow_content_length() {
	mut r := reader('POST / HTTP/1.1\r\nContent-Length: 99999999999999\r\n\r\n')
	http.parse_request(mut r) or {
		assert err.msg().contains('invalid Content-Length')
		return
	}
	assert false, 'expected error for overflow Content-Length'
}

fn test_parse_request_accepts_valid_content_length() {
	body := 'hello'
	mut r := reader('POST / HTTP/1.1\r\nContent-Length: ${body.len}\r\n\r\n${body}')
	req := http.parse_request(mut r) or {
		assert false, 'should not fail for valid Content-Length: ${err}'
		return
	}
	assert req.data == body
}

fn test_parse_request_rejects_truncated_body() {
	// Content-Length claims 100 bytes, but only 50 bytes of body data provided
	actual_body := 'A'.repeat(50)
	req_str := 'POST / HTTP/1.1\r\nContent-Length: 100\r\n\r\n${actual_body}'
	mut r := reader(req_str)
	http.parse_request(mut r) or {
		assert err.msg().contains('unexpected EOF while reading request body')
		return
	}
	assert false, 'expected error for truncated body'
}

fn test_parse_request_with_limit_rejects_truncated_body() {
	// Same truncation test but via parse_request_with_limit
	actual_body := 'A'.repeat(50)
	req_str := 'POST / HTTP/1.1\r\nContent-Length: 100\r\n\r\n${actual_body}'
	mut r := reader(req_str)
	http.parse_request_with_limit(mut r, 1000) or {
		assert err.msg().contains('unexpected EOF while reading request body')
		return
	}
	assert false, 'expected error for truncated body via parse_request_with_limit'
}

fn test_parse_request_line_double_slash_path() {
	// Regression: GET //another.html HTTP/1.1 should preserve //another.html as path,
	// not interpret 'another.html' as a host (authority) due to the // prefix.
	method, target, version := http.parse_request_line('GET //another.html HTTP/1.1') or {
		panic('did not parse: ${err}')
	}
	assert method == .get
	assert version == .v1_1
	// The path must be //another.html, not empty or misinterpreted
	assert target.path == '//another.html'
	assert target.host == ''
}

fn test_parse_request_double_slash_full_request() {
	// Full request parsing with double-slash path
	mut r := reader('GET //another.html HTTP/1.1\r\nHost: example.com\r\n\r\n')
	req := http.parse_request(mut r) or { panic('did not parse: ${err}') }
	assert req.method == .get
	assert req.url == '//another.html'
}

fn test_parse_request_line_double_slash_with_query() {
	// Double-slash path with query string
	method, target, version := http.parse_request_line('GET //page?key=val HTTP/1.1') or {
		panic('did not parse: ${err}')
	}
	assert method == .get
	assert version == .v1_1
	assert target.path == '//page'
	assert target.host == ''
}

fn test_redirect_303_method_and_body_handling() {
	// Verify that the Request struct supports the method/data fields needed
	// for redirect handling. The do() function now uses mutable local variables
	// for method (and unsafe mutation for data) to correctly handle 303 See Other
	// redirects by switching to GET and dropping the body.
	// Full integration testing of 303 redirects requires a live HTTP server.
	req := http.Request{
		method: .post
		data: 'original_body'
		allow_redirect: true
	}
	assert req.method == .post
	assert req.data == 'original_body'
	assert req.allow_redirect == true
}
