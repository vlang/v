module http

import io

struct StringReader {
	text string
mut:
	place int
}

fn (mut s StringReader) read(mut buf []byte) ?int {
	if s.place >= s.text.len {
		return none
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
	parse_request(mut reader__) or { return }
	panic('should not have parsed')
}

fn test_parse_request_no_headers() {
	mut reader_ := reader('GET / HTTP/1.1\r\n\r\n')
	req := parse_request(mut reader_) or { panic('did not parse: $err') }
	assert req.method == .get
	assert req.url == '/'
	assert req.version == .v1_1
}

fn test_parse_request_two_headers() {
	mut reader_ := reader('GET / HTTP/1.1\r\nTest1: a\r\nTest2:  B\r\n\r\n')
	req := parse_request(mut reader_) or { panic('did not parse: $err') }
	assert req.header.custom_values('Test1') == ['a']
	assert req.header.custom_values('Test2') == ['B']
}

fn test_parse_request_two_header_values() {
	mut reader_ := reader('GET / HTTP/1.1\r\nTest1: a; b\r\nTest2: c\r\nTest2: d\r\n\r\n')
	req := parse_request(mut reader_) or { panic('did not parse: $err') }
	assert req.header.custom_values('Test1') == ['a; b']
	assert req.header.custom_values('Test2') == ['c', 'd']
}

fn test_parse_request_body() {
	mut reader_ := reader('GET / HTTP/1.1\r\nTest1: a\r\nTest2: b\r\nContent-Length: 4\r\n\r\nbodyabc')
	req := parse_request(mut reader_) or { panic('did not parse: $err') }
	assert req.data == 'body'
}

fn test_parse_request_line() {
	method, target, version := parse_request_line('GET /target HTTP/1.1') or {
		panic('did not parse: $err')
	}
	assert method == .get
	assert target.str() == '/target'
	assert version == .v1_1
}

fn test_parse_form() {
	assert parse_form('foo=bar&bar=baz') == {
		'foo': 'bar'
		'bar': 'baz'
	}
	assert parse_form('foo=bar=&bar=baz') == {
		'foo': 'bar='
		'bar': 'baz'
	}
	assert parse_form('foo=bar%3D&bar=baz') == {
		'foo': 'bar='
		'bar': 'baz'
	}
	assert parse_form('foo=b%26ar&bar=baz') == {
		'foo': 'b&ar'
		'bar': 'baz'
	}
	assert parse_form('a=b& c=d') == {
		'a':  'b'
		' c': 'd'
	}
	assert parse_form('a=b&c= d ') == {
		'a': 'b'
		'c': ' d '
	}
}

fn test_parse_multipart_form() {
	boundary := '6844a625b1f0b299'
	names := ['foo', 'fooz']
	file := 'bar.v'
	ct := 'application/octet-stream'
	contents := ['baz', 'buzz']
	data := "--$boundary
Content-Disposition: form-data; name=\"${names[0]}\"; filename=\"$file\"\r
Content-Type: $ct\r
\r
${contents[0]}\r
--$boundary\r
Content-Disposition: form-data; name=\"${names[1]}\"\r
\r
${contents[1]}\r
--$boundary--\r
"
	form, files := parse_multipart_form(data, boundary)
	assert files == {
		names[0]: [
			FileData{
				filename: file
				content_type: ct
				data: contents[0]
			},
		]
	}

	assert form == {
		names[1]: contents[1]
	}
}

fn test_multipart_form_body() {
	files := {
		'foo': [
			FileData{
				filename: 'bar.v'
				content_type: 'application/octet-stream'
				data: 'baz'
			},
		]
	}
	form := {
		'fooz': 'buzz'
	}

	body, boundary := multipart_form_body(form, files)
	parsed_form, parsed_files := parse_multipart_form(body, boundary)
	assert parsed_files == files
	assert parsed_form == form
}

fn test_parse_large_body() ? {
	body := 'A'.repeat(101) // greater than max_bytes
	req := 'GET / HTTP/1.1\r\nContent-Length: $body.len\r\n\r\n$body'
	mut reader_ := reader(req)
	result := parse_request(mut reader_) ?
	assert result.data.len == body.len
	assert result.data == body
}
