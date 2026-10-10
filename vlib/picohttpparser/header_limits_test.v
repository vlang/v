module picohttpparser

// Header-count limits and the header-name character set.

fn nth_header_name(i int) string {
	mut b := []u8{}
	b << `a` + u8(i / 26)
	b << `a` + u8(i % 26)
	return b.bytestr()
}

fn request_with_headers(count int) string {
	mut s := 'GET / HTTP/1.1\r\n'
	for i in 0 .. count {
		s += '${nth_header_name(i)}: v\r\n'
	}
	return s + '\r\n'
}

pub fn test_parse_request_accepts_99_headers() {
	mut r := Request{}
	n := r.parse_request(request_with_headers(99)) or {
		assert false, 'unexpected error: ${err}'
		0
	}
	assert n == 711, 'ret ${n}'
	assert r.num_headers == 99, 'num_headers ${r.num_headers}'
	assert r.headers[0].name == 'aa'
	assert r.headers[98].name == 'du'
	assert r.headers[0].value == 'v'
}

pub fn test_parse_request_rejects_100_headers() {
	mut r := Request{}
	mut got_err := ''
	n := r.parse_request(request_with_headers(100)) or {
		got_err = err.msg()
		-99
	}
	assert n == -99, 'ret ${n}'
	assert got_err == 'error parsing request: too many headers!', got_err
	// NOTE: max_headers is 100, but the counter is incremented before the end
	// of archive is seen, so 100 header lines is already one too many: the
	// 100th header is stored and the loop then reports the overflow.
	assert r.num_headers == 100, 'num_headers ${r.num_headers}'
	assert r.headers[0].name == 'aa'
	assert r.headers[99].name == 'dv'
}

pub fn test_parse_request_keeps_every_header_in_order() {
	mut r := Request{}
	n := r.parse_request('GET / HTTP/1.1\r\nHost: example.com\r\nAccept: text/html\r\nX-Custom-Key: 7\r\n\r\n') or {
		assert false, 'unexpected error: ${err}'
		0
	}
	assert n == 73, 'ret ${n}'
	assert r.num_headers == 3, 'num_headers ${r.num_headers}'
	for i in 0 .. r.num_headers {
		assert r.headers[i].name != '', 'header ${i} has no name'
	}
	assert r.headers[0].name == 'Host'
	assert r.headers[0].value == 'example.com'
	assert r.headers[1].name == 'Accept'
	assert r.headers[1].value == 'text/html'
	assert r.headers[2].name == 'X-Custom-Key'
	assert r.headers[2].value == '7'
}

pub fn test_parse_request_rejected_header_name_characters() {
	// The set token_char_map actually rejects, measured byte by byte over the
	// printable ASCII range 33..126, as the middle byte of the name "A<c>B".
	// NOTE: `:` cannot be probed this way — it terminates the name, so
	// `A:B: v` parses as the header `A` and is accepted either way.
	mut rejected := []u8{}
	for c in u8(33) .. u8(127) {
		if c == `:` {
			continue
		}
		mut raw := []u8{}
		raw << `A`
		raw << c
		raw << `B`
		raw << `:`
		raw << `v`
		mut r := Request{}
		mut ok := true
		r.parse_request('GET / HTTP/1.1\r\n' + raw.bytestr() + '\r\n\r\n') or { ok = false }
		if !ok {
			rejected << c
		}
	}
	// The table must describe exactly the 256 byte values, so it is 256 bytes
	// long: `\1` is not an escape in V and would occupy two bytes each.
	assert token_char_map.len == 256, 'token_char_map is ${token_char_map.len} bytes, want 256'
	// Everything outside the RFC 7230 tchar set must be rejected. tchar is
	// !#$%&'*+-.^_`|~ plus digits and letters.
	assert rejected.bytestr() == '"(),/;<=>?@[\\]{}', 'rejected=${rejected.bytestr()} want the non-tchar printable bytes'
}

pub fn test_parse_request_accepts_ordinary_header_names() {
	for name in ['Host', 'Content-Length', 'User-Agent', 'Accept-Encoding', 'X-Forwarded-For'] {
		mut r := Request{}
		n := r.parse_request('GET / HTTP/1.1\r\n${name}: v\r\n\r\n') or {
			assert false, 'unexpected error for ${name}: ${err}'
			0
		}
		assert n > 0, 'ret ${n}'
		assert r.num_headers == 1, 'num_headers ${r.num_headers}'
		assert r.headers[0].name == name
		assert r.headers[0].value == 'v'
	}
}
