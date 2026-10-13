module http

// The cookies of a request (`Request.add_cookie`, the `cookies` map that `http.fetch`
// fills) are written into one `Cookie` header as `name=value` pairs. These tests check
// that no name and no value can add a header line, split the header or add a pair.
//
// Two things are deliberately not pinned, because they belong to other helpers:
// whether a value with a space or comma inside it is quoted (`sanitize_cookie_value`),
// and which of the separators other than `;` and `=` a name may contain
// (`is_cookie_name_valid`). Such cases are compared after reading the header back.

const plain_request_lines = ['GET / HTTP/1.1', 'Host: example.com', 'User-Agent: v.http',
	'Content-Length: 0', 'Connection: close']

// h1_request returns the bytes of the HTTP/1.1 request that `req` sends.
fn h1_request(req Request) string {
	return req.build_request_headers(.get, 'example.com', 80, '/') or { panic(err) }
}

// h1_expected returns the request that h1_request produces for a request that has
// nothing but the given Cookie header line (an empty string for no line at all).
fn h1_expected(cookie_line string) string {
	return 'GET / HTTP/1.1\r\nHost: example.com\r\nUser-Agent: v.http\r\nContent-Length: 0\r\n${cookie_line}Connection: close\r\n\r\n'
}

// sent_cookies checks that the request `req` sends has the lines of a request
// without cookies plus at most one Cookie line, and returns the cookies that the
// server side of this module reads from it, as `name=value` strings.
fn sent_cookies(req Request) []string {
	raw := h1_request(req)
	assert raw.ends_with('\r\n\r\n'), raw.bytes().str()
	lines := raw.all_before('\r\n\r\n').split('\r\n')
	for line in lines {
		assert !line.contains_any('\r\n'), raw.bytes().str()
	}
	assert lines.filter(!it.starts_with('Cookie: ')) == plain_request_lines, raw.bytes().str()
	assert lines.len <= plain_request_lines.len + 1, raw.bytes().str()
	parsed := parse_request_head_str(raw) or { panic(err) }
	return read_cookies(parsed.header, '').map('${it.name}=${it.value}')
}

fn request_with_cookie(name string, value string) Request {
	mut req := Request{}
	req.add_cookie(Cookie{ name: name, value: value })
	return req
}

fn test_ordinary_request_cookies_are_written_unchanged() {
	for value in ['abc123', '', 'dGVzdA==', 'a+b/c=d', 'eyJhbGciOiJIUzI1NiJ9.e30.x-y_z', 'a%20b%3B',
		'1|2|3', "it's", '[1:2]{3}<4>(5)?@*!#$&^`~'] {
		req := request_with_cookie('sid', value)
		assert req.cookie_header_value() == 'sid=${value}'
		assert h1_request(req) == h1_expected('Cookie: sid=${value}\r\n')
		assert sent_cookies(req) == ['sid=${value}']
	}
	assert h1_request(Request{}) == h1_expected('')
}

fn test_request_cookies_keep_their_order_and_come_before_cookie_header_values() {
	mut req := Request{}
	req.add_cookie(Cookie{ name: 'zeta', value: '1' })
	req.add_cookie(Cookie{ name: 'alpha', value: '2' })
	req.add_cookie(Cookie{ name: 'mid', value: '3' })
	assert req.cookie_header_value() == 'zeta=1; alpha=2; mid=3'
	// Explicit Cookie header values are appended as they are.
	req.header.add(.cookie, 'theme=dark; lang=en')
	req.header.add_custom('cookie', 'k="q"')!
	assert req.cookie_header_value() == 'zeta=1; alpha=2; mid=3; theme=dark; lang=en; k="q"'
	assert h1_request(req) == h1_expected('Cookie: zeta=1; alpha=2; mid=3; theme=dark; lang=en; k="q"\r\n')
	assert sent_cookies(req) == ['zeta=1', 'alpha=2', 'mid=3', 'theme=dark', 'lang=en', 'k=q']
}

fn test_request_cookie_name_with_crlf_adds_no_header_line() {
	for name in ['x\r\nInjected: 1', 'x\r\nInjected:1', 'x\nInjected:1', 'x\rInjected:1', '\r\n',
		'x\r\n\r\nbody'] {
		req := request_with_cookie(name, 'v')
		assert req.cookie_header_value() == '', name.bytes().str()
		assert h1_request(req) == h1_expected(''), name.bytes().str()
		assert sent_cookies(req) == []
		// The other cookies of the request are still sent.
		mut several := Request{}
		several.add_cookie(Cookie{ name: 'first', value: '1' })
		several.add_cookie(Cookie{ name: name, value: 'v' })
		several.add_cookie(Cookie{ name: 'last', value: '2' })
		assert h1_request(several) == h1_expected('Cookie: first=1; last=2\r\n'), name.bytes().str()
	}
}

fn test_request_cookie_value_with_crlf_adds_no_header_line() {
	req := request_with_cookie('c', 'x\r\nInjected:1')
	assert req.cookie_header_value() == 'c=xInjected:1'
	assert h1_request(req) == h1_expected('Cookie: c=xInjected:1\r\n')
	for value, want in {
		'x\r\nInjected: 1': 'xInjected: 1'
		'x\nInjected: 1':   'xInjected: 1'
		'x\rInjected: 1':   'xInjected: 1'
		'x\r\n\r\nbody':    'xbody'
	} {
		assert sent_cookies(request_with_cookie('c', value)) == ['c=${want}'], value.bytes().str()
	}
}

fn test_request_cookie_value_cannot_add_a_cookie() {
	req := request_with_cookie('c', 'a;admin=1')
	assert req.cookie_header_value() == 'c=aadmin=1'
	assert h1_request(req) == h1_expected('Cookie: c=aadmin=1\r\n')
	assert sent_cookies(req) == ['c=aadmin=1']
	assert sent_cookies(request_with_cookie('c', 'a; admin=1')) == ['c=a admin=1']
	assert sent_cookies(request_with_cookie('c', ';')) == ['c=']
	// A quote cannot close a quoted value early either.
	assert sent_cookies(request_with_cookie('c', ' a"; admin=1')) == ['c= a admin=1']
}

fn test_request_cookie_value_drops_the_bytes_a_cookie_value_cannot_hold() {
	for value, want in {
		'a"b':          'ab'
		'"abc"':        'abc'
		'a\\b':         'ab'
		'caf\xc3\xa9':  'caf'
		'\xff\xfe':     ''
		'a\x00b':       'ab'
		'a\tb':         'ab'
		'a\x7fb':       'ab'
		'\x01\x1f\x7f': ''
	} {
		req := request_with_cookie('c', value)
		assert req.cookie_header_value() == 'c=${want}', value.bytes().str()
		assert h1_request(req) == h1_expected('Cookie: c=${want}\r\n'), value.bytes().str()
		assert sent_cookies(req) == ['c=${want}'], value.bytes().str()
	}
}

fn test_request_cookie_value_keeps_spaces_and_commas() {
	// A space or comma at either end needs quotes to survive; the reader strips them.
	for value in [' a', 'a ', ',a', 'a,', ' ', ','] {
		req := request_with_cookie('c', value)
		assert req.cookie_header_value() == 'c="${value}"'
		assert sent_cookies(req) == ['c=${value}']
	}
	for value in ['a b', 'a,b', 'Mon, 02 Jan 2006', 'a, b c'] {
		req := request_with_cookie('c', value)
		assert req.cookie_header_value() in ['c=${value}', 'c="${value}"']
		assert sent_cookies(req) == ['c=${value}']
	}
}

fn test_request_cookie_with_an_unusable_name_is_left_out() {
	for name in ['', ' ', 'a b', ' a', 'a ', 'a;b', 'a; admin', ';', 'a=b', 'a=1; admin', '=',
		'a\tb', 'a\x00b', '\x1f', 'a\x7f', 'na\xc3\xafve', '\xff'] {
		req := request_with_cookie(name, '1')
		assert req.cookie_header_value() == '', name.bytes().str()
		assert h1_request(req) == h1_expected(''), name.bytes().str()
		mut several := Request{}
		several.add_cookie(Cookie{ name: 'first', value: '1' })
		several.add_cookie(Cookie{ name: name, value: '1' })
		several.add_cookie(Cookie{ name: 'last', value: '2' })
		assert several.cookie_header_value() == 'first=1; last=2', name.bytes().str()
		assert sent_cookies(several) == ['first=1', 'last=2'], name.bytes().str()
	}
}

fn test_request_cookie_token_name_is_written_unchanged() {
	for name in ['sid', 'SID', '__Host-id', '__Secure-id', 'a.b', 'a-b_c', '0', "!#$%&'*+-.^_`|~",
		'azAZ09'] {
		req := request_with_cookie(name, 'v')
		assert req.cookie_header_value() == '${name}=v'
		assert h1_request(req) == h1_expected('Cookie: ${name}=v\r\n')
		assert sent_cookies(req) == ['${name}=v']
	}
}

fn test_request_cookie_name_and_value_with_any_byte() {
	for i in 0 .. 256 {
		b := u8(i)
		// As part of a name: the pair is left out, or it is read back as that one pair.
		name := [u8(`a`), b, `b`].bytestr()
		by_name := request_with_cookie(name, 'v')
		got_name := by_name.cookie_header_value()
		assert got_name in ['', '${name}=v'], 'name byte ${i}: ${got_name.bytes()}'
		if b <= 0x20 || b >= 0x7f || b in [`;`, `=`] {
			assert got_name == '', 'name byte ${i}: ${got_name.bytes()}'
		}
		if b.is_alnum() || b in [`-`, `_`, `.`] {
			assert got_name == '${name}=v', 'name byte ${i}'
		}
		want_name := if got_name == '' { []string{} } else { [got_name] }
		assert sent_cookies(by_name) == want_name, 'name byte ${i}'

		// As part of a value: the byte is dropped or kept, and one pair is read back.
		value := [u8(`x`), b, `y`].bytestr()
		by_value := request_with_cookie('c', value)
		got_value := by_value.cookie_header_value()
		assert got_value in ['c=xy', 'c=${value}', 'c="${value}"'], 'value byte ${i}: ${got_value.bytes()}'
		if b < 0x20 || b >= 0x7f || b in [`;`, `"`, `\\`] {
			assert got_value == 'c=xy', 'value byte ${i}: ${got_value.bytes()}'
		}
		if b.is_alnum() || b in [`=`, `-`, `_`, `.`, `/`, `+`, `%`] {
			assert got_value == 'c=${value}', 'value byte ${i}'
		}
		want_value := if got_value == 'c=xy' { 'c=xy' } else { 'c=${value}' }
		assert sent_cookies(by_value) == [want_value], 'value byte ${i}'
	}
}

fn test_request_cookies_map_is_sanitized() {
	req := Request{
		cookies: {
			'sid':              'abc'
			'x\r\nInjected: 1': 'v'
			'c':                'x\r\nInjected:1'
			'a; admin':         '1'
			'd':                'a;admin=1'
		}
	}
	assert req.cookie_header_value() == 'sid=abc; c=xInjected:1; d=aadmin=1'
	assert h1_request(req) == h1_expected('Cookie: sid=abc; c=xInjected:1; d=aadmin=1\r\n')
	assert sent_cookies(req) == ['sid=abc', 'c=xInjected:1', 'd=aadmin=1']

	mut filled := Request{}
	filled.cookies['x\r\nInjected: 1'] = 'v'
	filled.cookies['c'] = 'v\r\nInjected:1'
	assert h1_request(filled) == h1_expected('Cookie: c=vInjected:1\r\n')
}

fn test_fetch_config_cookies_are_sanitized() {
	// `http.fetch` sends the request that `http.prepare` returns.
	req := prepare(
		url:     'http://example.com/'
		cookies: {
			'sid':              'abc'
			'x\r\nInjected: 1': 'v'
			'c':                'x\r\nInjected:1'
			'a=1; admin':       '1'
			'd':                'a;admin=1'
		}
	)!
	assert req.cookie_header_value() == 'sid=abc; c=xInjected:1; d=aadmin=1'
	assert h1_request(req) == h1_expected('Cookie: sid=abc; c=xInjected:1; d=aadmin=1\r\n')
	assert sent_cookies(req) == ['sid=abc', 'c=xInjected:1', 'd=aadmin=1']
}

fn test_pooled_request_without_connection_close_is_sanitized() {
	req := Request{
		cookies: {
			'x\r\nInjected: 1': 'v'
			'c':                'x\r\nInjected:1'
		}
	}
	raw := req.build_request_headers_opts(.get, 'example.com', 80, 80, '/', '', req.header, false)!
	assert raw == 'GET / HTTP/1.1\r\nHost: example.com\r\nUser-Agent: v.http\r\nContent-Length: 0\r\nCookie: c=xInjected:1\r\n\r\n'
}

fn test_request_cookie_returns_the_cookie_as_it_was_added() {
	mut req := Request{}
	req.add_cookie(Cookie{ name: 'c', value: 'a;b"c\r\n' })
	req.add_cookie(Cookie{ name: 'a b', value: 'v' })
	stored := req.cookie('c') or { panic('cookie `c` is missing') }
	assert stored.value == 'a;b"c\r\n'
	unusable := req.cookie('a b') or { panic('cookie `a b` is missing') }
	assert unusable.value == 'v'
	assert req.cookie_header_value() == 'c=abc'
}

fn test_to_h2_request_sanitizes_request_cookies() {
	req := Request{
		cookies: {
			'sid':              'abc'
			'x\r\nInjected: 1': 'v'
			'c':                'x\r\nInjected:1'
			'a; admin':         '1'
			'd':                'a;admin=1'
		}
	}
	mut header := new_header()
	header.add(.cookie, 'theme=dark')
	h2req := req.to_h2_request(.get, 'example.com', '/', '', header)
	cookie := h2req.headers.filter(it.name == 'cookie')
	assert cookie.len == 1
	assert cookie[0].value == 'sid=abc; c=xInjected:1; d=aadmin=1; theme=dark'
	for field in h2req.headers {
		assert !field.name.contains_any('\r\n'), field.name.bytes().str()
		assert !field.value.contains_any('\r\n'), field.value.bytes().str()
	}

	// A request whose cookies are all left out has no cookie field at all.
	unusable := Request{
		cookies: {
			'x\r\nInjected: 1': 'v'
		}
	}
	h2none := unusable.to_h2_request(.get, 'example.com', '/', '', new_header())
	assert !h2none.headers.any(it.name == 'cookie')
}
