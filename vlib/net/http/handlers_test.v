module http

struct DummyHandler {}

fn (d DummyHandler) handle(req Request) Response {
	mut r := Response{}
	r.body = req.url
	return r
}

fn test_redirect() {
	for code in [301, 302, 303, 307, 308] {
		r := redirect('/foo?next=%2Fbar', code)
		assert r.status_code == code
		assert r.http_version == '1.1'
		assert (r.header.get(.location) or { '' }) == '/foo?next=%2Fbar'
		assert (r.header.get(.content_length) or { '' }) == '0'
		parsed := (parse_response(r.bytestr()) or { panic(err) })
		assert parsed.status_code == code
		assert parsed.body == ''
	}
}

fn test_not_found() {
	r := not_found()
	assert r.status_code == 404
	assert r.body == '404 page not found'
	assert (r.header.get(.content_type) or { '' }) == 'text/plain; charset=utf-8'
	assert (r.header.get(.x_content_type_options) or { '' }) == 'nosniff'
	assert (r.header.get(.content_length) or { '' }) == r.body.len.str()
	assert (parse_response(r.bytestr()) or { panic(err) }).status_code == 404
}

fn test_error_response() {
	r := error_response('boom', 500)
	assert r.status_code == 500
	assert r.body == 'boom'
	assert r.http_version == '1.1'
	assert (r.header.get(.content_type) or { '' }) == 'text/plain; charset=utf-8'
	assert (r.header.get(.x_content_type_options) or { '' }) == 'nosniff'
	assert (r.header.get(.content_length) or { '' }) == '4'
}

fn test_response_helpers_preserve_unassigned_status_codes() {
	for r in [redirect('/next', 399), error_response('custom failure', 599)] {
		assert r.status_code in [399, 599]
		assert r.status_msg == ''
		assert (parse_response(r.bytestr()) or { panic(err) }).status_code == r.status_code
	}
}

fn test_error_response_preserves_body_and_counts_bytes() {
	for body in ['', 'échec\n<failure>'] {
		r := error_response(body, 400)
		assert r.body == body
		assert (r.header.get(.content_length) or { '' }) == body.len.str()
		assert (parse_response(r.bytestr()) or { panic(err) }).body == body
	}
}

fn test_strip_prefix() {
	mut h := strip_prefix('/api', DummyHandler{})
	resp := h.handle(new_request(.get, '/api/foo', ''))
	assert resp.body == '/foo?'
	resp2 := h.handle(new_request(.get, '/other', ''))
	assert resp2.status_code == 404
}

fn test_strip_prefix_preserves_query_and_escaped_suffix() {
	mut h := strip_prefix('/api', DummyHandler{})
	req := Request{
		url: '/api/a%2Fb?next=/api&x=%2F'
	}
	assert h.handle(req).body == '/a%2Fb?next=/api&x=%2F'
	assert req.url == '/api/a%2Fb?next=/api&x=%2F'
}

fn test_strip_prefix_matches_path_of_absolute_url() {
	mut h := strip_prefix('/api', DummyHandler{})
	assert h.handle(Request{ url: 'https://example.com/api/foo?x=1' }).body == 'https://example.com/foo?x=1'
}

fn test_strip_prefix_treats_leading_double_slash_as_request_path() {
	mut h := strip_prefix('//api', DummyHandler{})
	assert h.handle(Request{ url: '//api/foo?x=1' }).body == '/foo?x=1'
	mut slash := strip_prefix('/', DummyHandler{})
	assert slash.handle(Request{ url: '//foo' }).body == '/foo'
}

fn test_strip_prefix_normalizes_empty_path() {
	mut h := strip_prefix('/api', DummyHandler{})
	for url in ['/api', '/api?', '/api?x=1'] {
		assert h.handle(Request{ url: url }).body == '/' + url['/api'.len..]
	}
}

fn test_strip_prefix_does_not_match_query_or_escaped_prefix() {
	mut h := strip_prefix('/api', DummyHandler{})
	for url in ['/other?next=/api', '/%61pi/foo', '/api%2Ffoo', '/api/%zz'] {
		if url == '/api%2Ffoo' {
			// The prefix itself is literal; an escaped slash in the suffix is retained.
			assert h.handle(Request{ url: url }).body == '%2Ffoo'
		} else {
			assert h.handle(Request{ url: url }).status_code == 404
		}
	}
	mut query_prefix := strip_prefix('/api?x=', DummyHandler{})
	assert query_prefix.handle(Request{ url: '/api?x=1' }).status_code == 404
}

fn test_strip_prefix_empty_prefix_preserves_request() {
	mut h := strip_prefix('', DummyHandler{})
	for url in ['', '/?x=1', '/%zz', 'https://example.com/'] {
		assert h.handle(Request{ url: url }).body == url
	}
}

struct PrefixCountingHandler {
mut:
	calls int
}

fn (mut h PrefixCountingHandler) handle(req Request) Response {
	h.calls++
	return new_response(body: '${h.calls}:${req.method}:${req.data}:${req.url}')
}

fn test_strip_prefix_calls_mutable_handler_only_for_matches() {
	mut h := strip_prefix('/api', PrefixCountingHandler{})
	assert h.handle(Request{ url: '/outside' }).status_code == 404
	for call in 1 .. 3 {
		resp := h.handle(Request{ method: .post, data: 'payload', url: '/api/foo' })
		assert resp.body == '${call}:POST:payload:/foo'
	}
}
