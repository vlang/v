module vweb

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
	n := copy(buf, s.text[s.place..].bytes())
	s.place += n
	return n
}

fn reader(s string) &io.BufferedReader {
	return io.new_buffered_reader(reader: io.make_reader(&StringReader{ text: s }))
}

fn test_parse_request_not_http() {
	parse_request(mut reader('hello')) or { return }
	panic('should not have parsed')
}

fn test_parse_request_no_headers() {
	req := parse_request(mut reader('GET / HTTP/1.1\r\n\r\n')) or { panic('did not parse: $err') }
	assert req.method == .get
	assert req.url == '/'
	assert req.version == .v1_1
}

fn test_parse_request_two_headers() {
	req := parse_request(mut reader('GET / HTTP/1.1\r\nTest1: a\r\nTest2:  B\r\n\r\n')) or {
		panic('did not parse: $err')
	}
	assert req.headers == map{
		'Test1': 'a'
		'Test2': 'B'
	}
	assert req.lheaders == map{
		'test1': 'a'
		'test2': 'B'
	}
}

fn test_parse_request_two_header_values() {
	req := parse_request(mut reader('GET / HTTP/1.1\r\nTest1: a; b\r\nTest2: c\r\nTest2: d\r\n\r\n')) or {
		panic('did not parse: $err')
	}
	assert req.headers == map{
		'Test1': 'a; b'
		'Test2': 'c; d'
	}
	assert req.lheaders == map{
		'test1': 'a; b'
		'test2': 'c; d'
	}
}

fn test_parse_request_body() {
	req := parse_request(mut reader('GET / HTTP/1.1\r\nTest1: a\r\nTest2: b\r\nContent-Length: 4\r\n\r\nbodyabc')) or {
		panic('did not parse: $err')
	}
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
	assert parse_form('foo=bar&bar=baz') == map{
		'foo': 'bar'
		'bar': 'baz'
	}
	assert parse_form('foo=bar=&bar=baz') == map{
		'foo': 'bar='
		'bar': 'baz'
	}
	assert parse_form('foo=bar%3D&bar=baz') == map{
		'foo': 'bar='
		'bar': 'baz'
	}
	assert parse_form('foo=b%26ar&bar=baz') == map{
		'foo': 'b&ar'
		'bar': 'baz'
	}
	assert parse_form('a=b& c=d') == map{
		'a':  'b'
		' c': 'd'
	}
	assert parse_form('a=b&c= d ') == map{
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
	data := '--------------------------$boundary
Content-Disposition: form-data; name=\"${names[0]}\"; filename=\"$file\"
Content-Type: $ct

${contents[0]}
--------------------------$boundary
Content-Disposition: form-data; name=\"${names[1]}\"

${contents[1]}
--------------------------$boundary--
'
	form, files := parse_multipart_form(data, boundary)
	// TODO: remove newlines
	assert files == map{
		names[0]: [FileData{
			filename: file
			content_type: ct
			data: contents[0] + '\n'
		}]
	}

	assert form == map{
		names[1]: contents[1] + '\n'
	}
}
