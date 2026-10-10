import net.http

struct CookieValueCase {
	value   string // the value given to the cookie
	written string // what `sanitize_cookie_value` returns and `str()` writes after `name=`
	read    string // the value a reader gets back from `written`
}

// The expected results are the ones of Go's net/http `sanitizeCookieValue`:
// invalid bytes are dropped, and what is left is wrapped in double quotes
// when it contains a space or a comma.
const cookie_value_cases = [
	CookieValueCase{ value: 'a z', written: '"a z"', read: 'a z' },
	CookieValueCase{ value: 'a"b', written: 'ab', read: 'ab' },
	CookieValueCase{ value: 'é', written: '', read: '' },
	CookieValueCase{ value: 'a;b', written: 'ab', read: 'ab' },
	CookieValueCase{ value: 'a,b', written: '"a,b"', read: 'a,b' },
	// a space or a comma at either end
	CookieValueCase{ value: ' z', written: '" z"', read: ' z' },
	CookieValueCase{ value: 'a ', written: '"a "', read: 'a ' },
	CookieValueCase{ value: ' ', written: '" "', read: ' ' },
	CookieValueCase{ value: ',z', written: '",z"', read: ',z' },
	CookieValueCase{ value: 'a,', written: '"a,"', read: 'a,' },
	CookieValueCase{ value: ',', written: '","', read: ',' },
	// more than one space or comma
	CookieValueCase{ value: 'a b c', written: '"a b c"', read: 'a b c' },
	CookieValueCase{ value: 'a, b', written: '"a, b"', read: 'a, b' },
	// bytes that are not allowed in a cookie value are dropped
	CookieValueCase{ value: 'a\\b', written: 'ab', read: 'ab' },
	CookieValueCase{ value: '\x00a\tb\r\nc', written: 'abc', read: 'abc' },
	CookieValueCase{ value: 'a\x7fb', written: 'ab', read: 'ab' },
	CookieValueCase{ value: [u8(0), 0x7e, 0x7f, 0x80, 0xff].bytestr(), written: '~', read: '~' },
	CookieValueCase{ value: 'aéb', written: 'ab', read: 'ab' },
	// the quotes are decided on what is left after the invalid bytes are dropped
	CookieValueCase{ value: 'a b,c;d', written: '"a b,cd"', read: 'a b,cd' },
	CookieValueCase{ value: 'a;b c', written: '"ab c"', read: 'ab c' },
	CookieValueCase{ value: '"a z"', written: '"a z"', read: 'a z' },
	CookieValueCase{ value: '"a"', written: 'a', read: 'a' },
	// nothing to quote
	CookieValueCase{ value: '', written: '', read: '' },
	CookieValueCase{ value: '";', written: '', read: '' },
	CookieValueCase{ value: '\t\n', written: '', read: '' },
	CookieValueCase{ value: 'plain', written: 'plain', read: 'plain' },
	CookieValueCase{ value: 'a=b&c=d', written: 'a=b&c=d', read: 'a=b&c=d' },
]

// set_cookie_value returns the value of the cookie `c`, read from a `Set-Cookie: ${line}` response header.
fn set_cookie_value(line string) !string {
	resp := http.parse_response('HTTP/1.1 200 OK\r\nSet-Cookie: ${line}\r\nContent-Length: 0\r\n\r\n')!
	cookies := resp.cookies()
	if cookies.len != 1 || cookies[0].name != 'c' {
		return error('expected the cookie `c` in `Set-Cookie: ${line}`, got ${cookies}')
	}
	return cookies[0].value
}

// request_cookie_value returns the value of the cookie `c`, read from a `Cookie: ${line}` request header.
fn request_cookie_value(line string) !string {
	req := http.parse_request_head_str('GET / HTTP/1.1\r\nHost: example.com\r\nCookie: ${line}\r\n\r\n')!
	cookie := req.cookie('c') or { return error('no cookie `c` in `Cookie: ${line}`') }
	return cookie.value
}

fn test_sanitize_cookie_value_drops_invalid_bytes_and_quotes_spaces_and_commas() {
	for tt in cookie_value_cases {
		assert http.sanitize_cookie_value(tt.value) == tt.written, 'value: `${tt.value}`'
	}
}

fn test_cookie_str_writes_the_sanitized_value() {
	for tt in cookie_value_cases {
		cookie := http.Cookie{
			name:  'c'
			value: tt.value
		}
		assert cookie.str() == 'c=${tt.written}', 'value: `${tt.value}`'
	}
}

fn test_cookie_str_never_writes_a_semicolon_from_the_value() {
	for value in ['a;b', 'a; b', ';', 'a;b c', '"a;b"', 'a; Path=/admin'] {
		cookie := http.Cookie{
			name:  'c'
			value: value
		}
		assert !cookie.str().contains(';'), 'value: `${value}`'
	}
}

fn test_written_cookie_value_reads_back_as_the_sanitized_value() {
	for tt in cookie_value_cases {
		line := http.Cookie{
			name:  'c'
			value: tt.value
		}.str()
		assert line == 'c=${tt.written}', 'value: `${tt.value}`'
		assert set_cookie_value(line)! == tt.read, 'Set-Cookie: ${line}'
		assert request_cookie_value(line)! == tt.read, 'Cookie: ${line}'
		// writing what was read gives the same header again
		again := http.Cookie{
			name:  'c'
			value: tt.read
		}
		assert again.str() == line, 'value: `${tt.value}`'
	}
}

fn test_quoted_cookie_value_is_read_without_its_quotes() {
	assert set_cookie_value('c="a z"')! == 'a z'
	assert set_cookie_value('c="a,b"')! == 'a,b'
	assert set_cookie_value('c=""')! == ''
	assert request_cookie_value('c="a z"')! == 'a z'
	assert request_cookie_value('c="a,b"')! == 'a,b'
	assert request_cookie_value('c=""')! == ''
	// several cookies in one request header
	req := http.parse_request_head_str('GET / HTTP/1.1\r\nHost: example.com\r\nCookie: a="x y"; c="a,b"; d=plain\r\n\r\n')!
	assert req.cookie('a')?.value == 'x y'
	assert req.cookie('c')?.value == 'a,b'
	assert req.cookie('d')?.value == 'plain'
}

fn test_quoted_cookie_value_is_followed_by_the_attributes() {
	line := http.Cookie{
		name:      'c'
		value:     'a z'
		path:      '/p'
		http_only: true
	}.str()
	assert line.starts_with('c="a z"; '), line
	resp := http.parse_response('HTTP/1.1 200 OK\r\nSet-Cookie: ${line}\r\nContent-Length: 0\r\n\r\n')!
	cookies := resp.cookies()
	assert cookies.len == 1
	assert cookies[0].name == 'c'
	assert cookies[0].value == 'a z'
	assert cookies[0].path == '/p'
	assert cookies[0].http_only
}
