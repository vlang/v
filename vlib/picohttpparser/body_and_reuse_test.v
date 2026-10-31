module picohttpparser

// Body extraction and reuse of one `Request` across several calls, which is
// what `prev_len` (the slowloris counter) exists for.

struct BodyCase {
	input string
	ret   int
	body  string
}

const body_cases = [
	BodyCase{
		input: 'POST / HTTP/1.1\r\nContent-Length: 10\r\n\r\nsomedata'
		ret:   39
		body:  'somedata'
	},
	// Content-Length is not used to bound the body: everything after the blank
	// line becomes `body`, longer or shorter than the header claims.
	BodyCase{
		input: 'POST / HTTP/1.1\r\nContent-Length: 4\r\n\r\nabcdef'
		ret:   38
		body:  'abcdef'
	},
	BodyCase{
		input: 'POST / HTTP/1.1\r\nContent-Length: 10\r\n\r\nabc'
		ret:   39
		body:  'abc'
	},
	BodyCase{
		input: 'POST / HTTP/1.1\r\nContent-Length: abc\r\n\r\nxyz'
		ret:   40
		body:  'xyz'
	},
	BodyCase{
		input: 'POST / HTTP/1.1\r\nContent-Length: 99999999\r\n\r\nabc'
		ret:   45
		body:  'abc'
	},
	BodyCase{
		input: 'POST / HTTP/1.1\r\nHost: x\r\n\r\nsomedata'
		ret:   28
		body:  'somedata'
	},
	BodyCase{
		input: 'POST / HTTP/1.1\r\nContent-Length: 0\r\n\r\n'
		ret:   38
		body:  ''
	},
	// CRLF inside the body survives; only the headers are line-parsed
	BodyCase{
		input: 'POST / HTTP/1.1\r\nContent-Length: 7\r\n\r\na\r\nb\r\nc'
		ret:   38
		body:  'a\r\nb\r\nc'
	},
]

pub fn test_parse_request_body_table() {
	for c in body_cases {
		mut r := Request{}
		mut got_err := ''
		n := r.parse_request(c.input) or {
			got_err = err.msg()
			-99
		}
		assert got_err == '', 'input "${c.input}": ${got_err}'
		assert n == c.ret, 'input "${c.input}": ret ${n}, want ${c.ret}'
		assert r.body == c.body, 'input "${c.input}": body "${r.body}", want "${c.body}"'
	}
}

pub fn test_parse_request_body_keeps_binary_bytes() {
	mut raw := []u8{}
	for c in 'POST / HTTP/1.1\r\nContent-Length: 3\r\n\r\n'.bytes() {
		raw << c
	}
	raw << 0
	raw << 1
	raw << 2

	mut r := Request{}
	mut got_err := ''
	n := r.parse_request(raw.bytestr()) or {
		got_err = err.msg()
		-99
	}
	assert got_err == '', got_err
	assert n == 38, 'ret ${n}'
	assert r.body.len == 3, 'body len ${r.body.len}'
	assert r.body[0] == `\0`
	assert r.body[1] == `\x01`
	assert r.body[2] == `\x02`
}

pub fn test_parse_request_sets_prev_len_to_the_buffer_length() {
	mut r := Request{}
	n := r.parse_request('GET / HTTP/1.1\r\nHost: x\r\n\r\n') or {
		assert false, 'unexpected error: ${err}'
		0
	}
	assert n == 27, 'ret ${n}'
	assert r.prev_len == 27, 'prev_len ${r.prev_len}'
	assert r.body == '', 'body "${r.body}"'
}

pub fn test_parse_request_reports_a_longer_incomplete_buffer_as_2() {
	// After one complete request, a longer buffer that still has no final blank
	// line goes through the is_complete path and returns -2.
	mut r := Request{}
	r.parse_request('GET / HTTP/1.1\r\nHost: x\r\n\r\n') or {
		assert false, 'unexpected error: ${err}'
	}
	mut got_err := ''
	n := r.parse_request('GET /partial HTTP/1.1\r\nHost: x') or {
		got_err = err.msg()
		-99
	}
	assert got_err == '', got_err
	assert n == -2, 'ret ${n}'
	assert r.prev_len == 27, 'prev_len ${r.prev_len}'
	assert r.method == 'GET'
	assert r.path == '/'
}

pub fn test_parse_request_parses_the_next_request_from_a_reused_request() {
	mut r := Request{}
	r.parse_request('GET / HTTP/1.1\r\nHost: x\r\n\r\n') or {
		assert false, 'unexpected error: ${err}'
	}
	mut got_err := ''
	n := r.parse_request('GET /p HTTP/1.1\r\nHost: y\r\n\r\n') or {
		got_err = err.msg()
		-99
	}
	assert got_err == '', got_err
	assert n == 28, 'ret ${n}'
	assert r.method == 'GET'
	assert r.path == '/p'
	assert r.body == '', 'body "${r.body}"'
	assert r.prev_len == 28, 'prev_len ${r.prev_len}'
	// NOTE: num_headers is cumulative across calls; a second parse appends to
	// the same fixed array instead of replacing the first request's headers.
	assert r.num_headers == 2, 'num_headers ${r.num_headers}'
	assert r.headers[0].name == 'Host'
	assert r.headers[0].value == 'x'
	assert r.headers[1].name == 'Host'
	assert r.headers[1].value == 'y'
}

pub fn test_parse_request_incomplete_first_call_leaves_prev_len_at_zero() {
	mut r := Request{}
	n := r.parse_request('GET / HT') or {
		assert false, 'unexpected error: ${err}'
		0
	}
	assert n == -2, 'ret ${n}'
	assert r.prev_len == 0, 'prev_len ${r.prev_len}'
	// the request line is fully read before the version is found missing
	assert r.method == 'GET'
	assert r.path == '/'
}
