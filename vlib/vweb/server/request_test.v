module server

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
	assert req.target.path == '/'
	assert req.version == .v1_1
}

fn test_parse_request_two_headers() {
	req := parse_request(mut reader('GET / HTTP/1.1\r\nTest1: a\r\nTest2:  b\r\n\r\n')) or {
		panic('did not parse: $err')
	}
	assert req.headers == map{
		'test1': ['a']
		'test2': ['b']
	}
}

fn test_parse_request_two_header_values() {
	req := parse_request(mut reader('GET / HTTP/1.1\r\nTest1: a; b\r\n\r\n')) or {
		panic('did not parse: $err')
	}
	assert req.headers == map{
		'test1': ['a', 'b']
	}
}

fn test_parse_request_body() {
	req := parse_request(mut reader('GET / HTTP/1.1\r\nTest1: a\r\nTest2: b\r\nContent-Length: 4\r\n\r\nbody')) or {
		panic('did not parse: $err')
	}
	assert req.body == 'body'.bytes()
}

fn test_parse_request_line() {
	method, target, version := parse_request_line('GET /target HTTP/1.1') or {
		panic('did not parse: $err')
	}
	assert method == .get
	assert target.str() == '/target'
	assert version == .v1_1
}
