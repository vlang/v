module http

import io

// A field value can not contain CR, LF or NUL (RFC 9110 section 5.5): written
// as they are, they end the field line. These tests cover every way a value
// gets into a `Header`, what the request and response writers send, and what
// the parsers accept.

// forbidden_bytes are the bytes that are kept out of a field value.
const forbidden_bytes = [u8(`\r`), `\n`, 0]

// allowed_bytes stay as they are: TAB, the other control bytes, which a
// recipient may keep, DEL, and obs-text. 0x8a, 0x8d and 0x80 are LF, CR and
// NUL with the high bit set.
const allowed_bytes = [u8(`\t`), 0x01, 0x0b, 0x0c, 0x0e, 0x1f, ` `, `a`, 0x7f, 0x80, 0x8a, 0x8d,
	0xc3, 0xff]

// with_byte returns `prefix`, the byte `b`, and `suffix`.
fn with_byte(prefix string, b u8, suffix string) string {
	mut bytes := prefix.bytes()
	bytes << b
	bytes << suffix.bytes()
	return bytes.bytestr()
}

// byte_name returns the name that `HeaderValueError.msg` uses for `b`.
fn byte_name(b u8) string {
	return match b {
		`\r` { 'CR' }
		`\n` { 'LF' }
		else { 'NUL' }
	}
}

// assert_value_error checks that `err` is the error for the byte `b` in a value of `key`.
fn assert_value_error(err IError, key string, b u8) {
	assert err is HeaderValueError, err.msg()
	if err is HeaderValueError {
		assert err.header == key
		assert err.invalid_char == b
	}
	assert err.msg() == "Invalid header value for '${key}': it contains ${byte_name(b)}"
}

struct StringReader {
	text string
mut:
	place int
}

fn (mut s StringReader) read(mut buf []u8) !int {
	if s.place >= s.text.len {
		return io.Eof{}
	}
	end := if s.place + 100 >= s.text.len { s.text.len } else { s.place + 100 }
	n := copy(mut buf, s.text[s.place..end].bytes())
	s.place += n
	return n
}

// buffered returns a reader of the raw message `s`.
fn buffered(s string) &io.BufferedReader {
	return io.new_buffered_reader(
		reader: &StringReader{
			text: s
		}
	)
}

// naive_index is the byte by byte scan that header_value_crlf_nul_index must agree with.
fn naive_index(value string) int {
	for i, c in value.bytes() {
		if c == `\r` || c == `\n` || c == 0 {
			return i
		}
	}
	return -1
}

fn test_crlf_nul_index_agrees_with_a_byte_by_byte_scan() {
	mut bytes := forbidden_bytes.clone()
	bytes << allowed_bytes
	// every length around the 8 byte words of the scan, every position, and
	// fillers that are below, at and above the 0x0e threshold of a word
	for filler in [u8(`x`), `\t`, 0x0e, 0x8d, 0xff] {
		for len in 0 .. 34 {
			mut value := []u8{len: len, init: filler}
			assert header_value_crlf_nul_index(value.bytestr()) == -1, '${filler} x ${len}'
			for pos in 0 .. len {
				for b in bytes {
					value[pos] = b
					s := value.bytestr()
					assert header_value_crlf_nul_index(s) == naive_index(s), '${s.bytes()}'
					value[pos] = filler
				}
			}
		}
	}
	// the first of several is reported
	assert header_value_crlf_nul_index('0123456789\n123456789\r1234\x00') == 10
	assert header_value_crlf_nul_index('\ta\tb\tc\td\te\tf\tg\th\r') == 16
}

fn test_writers_without_a_result_replace_cr_lf_and_nul_with_a_space() {
	for b in forbidden_bytes {
		bad := with_byte('a', b, 'X-Injected: 1')
		want := 'a X-Injected: 1'

		mut added := new_header()
		added.add(.location, bad)
		assert added.values(.location) == [want]

		mut set := new_header()
		set.set(.location, bad)
		assert set.values(.location) == [want]
		// a value that replaces another one
		set.set(.location, with_byte('b', b, ''))
		assert set.values(.location) == ['b ']

		assert new_header(key: .location, value: bad).values(.location) == [want]
		assert new_header(HeaderConfig{.accept, 'x'}, HeaderConfig{.location, bad}).values(.location) == [
			want,
		]
		assert new_header_from_map({
			CommonHeader.location: bad
		}).values(.location) == [want]

		mut mapped := new_header()
		mapped.add_map({
			CommonHeader.location: bad
		})
		assert mapped.values(.location) == [want]

		mut req := Request{}
		req.add_header(.location, bad)
		assert req.header.values(.location) == [want]

		for h in [added, set, mapped, req.header] {
			rendered := h.render()
			assert rendered.count('\n') == 1, rendered.bytes().str()
			assert !rendered.contains('\0')
			assert !rendered.contains('\nX-Injected')
		}
	}
	// every forbidden byte of a value is replaced, wherever it is
	mut h := new_header()
	h.add(.location, '\r\n/next\r\nSet-Cookie: sid=1\r\n\r\n<html>\x00\n')
	assert h.values(.location) == ['  /next  Set-Cookie: sid=1    <html>  ']
}

fn test_join_replaces_cr_lf_and_nul_with_a_space() {
	// Only a Header that was not filled by its own methods can hold such a
	// value. join must neither copy it nor panic on it.
	mut other := new_header(key: .accept, value: 'x')
	other.data[0] = HeaderKV{'X-Raw', 'a\r\nX-Injected: 1\x00'}
	joined := new_header(key: .accept, value: 'y').join(other)
	assert joined.custom_values('X-Raw') == ['a  X-Injected: 1 ']
	assert joined.values(.accept) == ['y']
}

fn test_writers_with_a_result_reject_cr_lf_and_nul() {
	for b in forbidden_bytes {
		for bad in [with_byte('a', b, 'X-Injected: 1'), with_byte('', b, ''),
			with_byte('0123456789abcdef', b, ''), with_byte('', b, '0123456789abcdef')] {
			mut h := new_header(key: .accept, value: 'x')
			h.add_custom('X-Old', 'old')!
			before := h.render()

			h.add_custom('X-Foo', bad) or { assert_value_error(err, 'X-Foo', b) }
			assert !h.contains_custom('X-Foo')

			h.set_custom('X-Foo', bad) or { assert_value_error(err, 'X-Foo', b) }
			assert !h.contains_custom('X-Foo')
			// the value that is present stays
			h.set_custom('X-Old', bad) or { assert_value_error(err, 'X-Old', b) }
			assert h.custom_values('X-Old') == ['old']

			h.add_custom_map({
				'X-Foo': bad
			}) or { assert_value_error(err, 'X-Foo', b) }
			assert h.render() == before

			if created := new_custom_header_from_map({
				'X-Foo': bad
			})
			{
				assert false, 'should have failed, got ${created.render().bytes()}'
			} else {
				assert_value_error(err, 'X-Foo', b)
			}

			mut req := Request{}
			req.add_custom_header('X-Foo', bad) or { assert_value_error(err, 'X-Foo', b) }
			assert req.header.keys() == []
		}
	}
}

fn test_the_error_names_the_field_and_the_byte_but_not_the_value() {
	mut h := new_header()
	mut failed := false
	h.add_custom('Authorization', 'Bearer s3cr3t-token\n') or {
		assert_value_error(err, 'Authorization', `\n`)
		assert !err.msg().contains('s3cr3t')
		failed = true
	}
	assert failed
	// the first forbidden byte is the one that is reported
	h.set_custom('X-Foo', with_byte('a', 0, 'b\rc\n')) or { assert_value_error(err, 'X-Foo', 0) }
	assert h.keys() == []
}

fn test_other_bytes_are_stored_and_rendered_as_they_are() {
	mut values := ['', ' ', 'text/html; charset=utf-8', '  leading and trailing \t ',
		'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36', 'h\xc3\xa9llo w\xf6rld \xff',
		'col1\tcol2\tcol3']
	for b in allowed_bytes {
		values << with_byte('a', b, 'b')
		values << with_byte('0123456789abcdef', b, '0123456789abcdef')
	}
	for value in values {
		mut h := new_header()
		h.add(.location, value)
		h.set(.etag, value)
		h.set(.etag, value)
		h.add_custom('X-Foo', value)!
		h.set_custom('X-Bar', value)!
		h.set_custom('X-Bar', value)!
		assert h.values(.location) == [value]
		assert h.values(.etag) == [value]
		assert h.custom_values('X-Foo') == [value]
		assert h.custom_values('X-Bar') == [value]
		assert h.render() == 'Location: ${value}\r\nETag: ${value}\r\nX-Foo: ${value}\r\nX-Bar: ${value}\r\n'
		assert new_header().join(h).render() == h.render()
		assert new_header(key: .location, value: value).render() == 'Location: ${value}\r\n'
		assert new_custom_header_from_map({
			'X-Foo': value
		})!.render() == 'X-Foo: ${value}\r\n'
	}
}

fn test_a_response_can_not_be_split_through_a_header_value() {
	mut resp := new_response(
		status: .found
		header: new_header(key: .location, value: '/next\r\nSet-Cookie: sid=1\r\n\r\n<html>')
		body:   'body'
	)
	resp.header.set(.content_type, 'text/plain\nX-Injected: 1')
	resp.header.add(.vary, 'Origin\rX-Injected: 2')
	resp.header.add(.etag, with_byte('a', 0, 'b'))
	expected := 'HTTP/1.1 302 Found\r\n' + 'Location: /next  Set-Cookie: sid=1    <html>\r\n' +
		'Content-Length: 4\r\n' + 'Content-Type: text/plain X-Injected: 1\r\n' +
		'Vary: Origin X-Injected: 2\r\n' + 'ETag: a b\r\n' + '\r\n' + 'body'
	assert resp.bytestr() == expected
	assert resp.bytes().bytestr() == expected
	mut sb := []u8{}
	resp.write_to(mut sb)
	assert sb.bytestr() == expected
	// what a client reads back is one response with these fields
	parsed := parse_response(expected)!
	assert parsed.header.keys() == ['Location', 'Content-Length', 'Content-Type', 'Vary', 'ETag']
	assert parsed.body == 'body'
}

fn test_a_request_can_not_be_split_through_a_header_value() {
	for b in forbidden_bytes {
		sep := b.ascii_str()
		mut req := Request{}
		req.header.add(.referer, 'a${sep}X-Injected: 1')
		req.header.set(.accept, '*/*${sep}${sep}GET /admin HTTP/1.1')
		req.header.add_custom('X-Rejected', 'a${sep}b') or { assert_value_error(err, 'X-Rejected', b) }
		raw := req.build_request_headers(.get, 'example.com', 80, '/x')!
		assert raw == 'GET /x HTTP/1.1\r\n' + 'Host: example.com\r\n' +
			'User-Agent: v.http\r\n' + 'Content-Length: 0\r\n' +
			'Referer: a X-Injected: 1\r\n' + 'Accept: */*  GET /admin HTTP/1.1\r\n' +
			'Connection: close\r\n' + '\r\n', raw.bytes().str()
		// the keep-alive form that the connection pool sends
		pooled := req.build_request_headers_opts(.get, 'example.com', 80, 80, '/x', '', req.header,
			false)!
		assert pooled == raw.replace('Connection: close\r\n', '')
	}
}

fn test_do_rejects_a_user_agent_with_cr_lf_or_nul() {
	// user_agent is written as a field value without being stored in a Header.
	// The request fails before a connection is opened, whatever the protocol.
	for b in forbidden_bytes {
		for enable_http2 in [false, true] {
			url := if enable_http2 { 'https://127.0.0.1:1/' } else { 'http://127.0.0.1:1/' }
			user_agent := with_byte('v.http', b, 'X-Injected: 1')
			req := Request{
				url:          url
				user_agent:   user_agent
				enable_http2: enable_http2
			}
			if resp := req.do() {
				assert false, 'sent, and got ${resp.status_code}'
			} else {
				assert_value_error(err, 'User-Agent', b)
			}
			if resp := fetch(url: url, user_agent: user_agent, enable_http2: enable_http2) {
				assert false, 'sent, and got ${resp.status_code}'
			} else {
				assert_value_error(err, 'User-Agent', b)
			}
		}
	}
}

fn test_an_explicit_cookie_value_can_not_split_a_request() {
	for b in forbidden_bytes {
		sep := b.ascii_str()
		// Cookie fields are not written by the loop that writes the other
		// fields, so what they send is checked on its own.
		mut req := Request{}
		req.header.add(.cookie, 'a=1${sep}X-Injected: 1')
		assert req.cookie_header_value() == 'a=1 X-Injected: 1'
		raw := req.build_request_headers(.get, 'example.com', 80, '/')!
		assert raw == 'GET / HTTP/1.1\r\n' + 'Host: example.com\r\n' + 'User-Agent: v.http\r\n' +
			'Content-Length: 0\r\n' + 'Cookie: a=1 X-Injected: 1\r\n' + 'Connection: close\r\n' +
			'\r\n', raw.bytes().str()

		mut rejected := Request{}
		rejected.header.add_custom('Cookie', 'a=1${sep}X-Injected: 1') or {
			assert_value_error(err, 'Cookie', b)
		}
		rejected.header.set_custom('cookie', 'a=1${sep}X-Injected: 1') or {
			assert_value_error(err, 'cookie', b)
		}
		assert rejected.cookie_header_value() == ''
		assert rejected.build_request_headers(.get, 'example.com', 80, '/')! == 'GET / HTTP/1.1\r\n' +
			'Host: example.com\r\n' + 'User-Agent: v.http\r\n' + 'Content-Length: 0\r\n' +
			'Connection: close\r\n' + '\r\n'
	}
}

fn test_an_http2_request_has_no_cr_lf_or_nul_in_a_field_value() {
	for b in forbidden_bytes {
		sep := b.ascii_str()
		req := Request{}
		mut header := new_header(key: .referer, value: 'a${sep}b')
		header.add(.cookie, 'c=1${sep}d')
		header.set(.host, 'example.org${sep}')
		h2req := req.to_h2_request(.get, 'example.com', '/', '', header)
		assert h2req.authority == 'example.org'
		assert h2req.headers.any(it.name == 'referer' && it.value == 'a b')
		assert h2req.headers.any(it.name == 'cookie' && it.value == 'c=1 d')
		for f in h2req.headers {
			assert header_value_crlf_nul_index(f.value) == -1, '${f.name}: ${f.value.bytes()}'
		}
	}
}

fn test_parsers_reject_a_field_value_with_a_bare_cr_or_a_nul() {
	// A line ends at LF, so LF is never part of a value that a parser sees.
	for b in [u8(`\r`), 0] {
		for value in [with_byte('a', b, 'b'), with_byte('a', b, ''),
			with_byte('0123456789abcdef', b, '0123456789abcdef')] {
			raw := 'GET /x HTTP/1.1\r\nHost: example.com\r\nX-Foo: ${value}\r\nX-After: 1\r\n\r\n'
			mut r := buffered(raw)
			if req := parse_request(mut r) {
				assert false, 'parse_request accepted ${req.header.render().bytes()}'
			} else {
				assert_value_error(err, 'X-Foo', b)
			}
			mut r2 := buffered(raw)
			if req := parse_request_head(mut r2) {
				assert false, 'parse_request_head accepted ${req.header.render().bytes()}'
			} else {
				assert_value_error(err, 'X-Foo', b)
			}
			if req := parse_request_head_str(raw) {
				assert false, 'parse_request_head_str accepted ${req.header.render().bytes()}'
			} else {
				assert_value_error(err, 'X-Foo', b)
			}
			if req := parse_request_str(raw) {
				assert false, 'parse_request_str accepted ${req.header.render().bytes()}'
			} else {
				assert_value_error(err, 'X-Foo', b)
			}
		}
	}
	// A leading NUL is not skipped the way leading whitespace is.
	if req := parse_request_head_str('GET / HTTP/1.1\r\nX-Foo: ${with_byte('', 0, 'ab')}\r\n\r\n') {
		assert false, 'parse_request_head_str accepted ${req.header.render().bytes()}'
	} else {
		assert_value_error(err, 'X-Foo', 0)
	}
	// The response parser ends a line at a bare CR as well, so only NUL can
	// reach a value there.
	for value in [with_byte('a', 0, 'b'), with_byte('', 0, 'ab'), with_byte('ab', 0, ''),
		with_byte('0123456789abcdef', 0, '0123456789abcdef')] {
		raw := 'HTTP/1.1 200 OK\r\nX-Foo: ${value}\r\nContent-Length: 0\r\n\r\n'
		if resp := parse_response(raw) {
			assert false, 'parse_response accepted ${resp.header.render().bytes()}'
		} else {
			assert_value_error(err, 'X-Foo', 0)
		}
		if h := parse_headers('X-Foo: ${value}\r\n') {
			assert false, 'parse_headers accepted ${h.render().bytes()}'
		} else {
			assert_value_error(err, 'X-Foo', 0)
		}
	}
	if resp := parse_response('HTTP/1.1 200 OK\r\nX-Foo: a\rb\r\nContent-Length: 0\r\n\r\n') {
		assert false, 'parse_response accepted ${resp.header.render().bytes()}'
	}
}

fn test_parsers_keep_the_other_bytes_of_a_field_value() {
	for b in allowed_bytes {
		value := with_byte('a', b, 'b')
		raw := 'GET /x HTTP/1.1\r\nHost: example.com\r\nX-Foo: ${value}\r\nX-After: 1\r\n\r\n'
		mut r := buffered(raw)
		assert parse_request(mut r)!.header.custom_values('X-Foo') == [value]
		assert parse_request_head_str(raw)!.header.custom_values('X-Foo') == [value]
		assert parse_request_str(raw)!.header.custom_values('X-After') == ['1']
		resp := parse_response('HTTP/1.1 200 OK\r\nX-Foo: ${value}\r\nContent-Length: 0\r\n\r\n')!
		assert resp.header.custom_values('X-Foo') == [value]
	}
}
