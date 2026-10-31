module http

struct DummyHandler {}

fn (d DummyHandler) handle(req Request) Response {
	mut r := Response{}
	r.body = req.url
	return r
}

fn test_redirect() {
	r := redirect('/foo', 302)
	assert r.status_code == 302
	loc := r.header.get(.location) or { '' }
	assert loc == '/foo'
}

fn test_not_found() {
	r := not_found()
	assert r.status_code == 404
}

fn test_error_response() {
	r := error_response('boom', 500)
	assert r.status_code == 500
	assert r.body == 'boom'
}

fn test_strip_prefix() {
	mut h := strip_prefix('/api', DummyHandler{})
	resp := h.handle(new_request(.get, '/api/foo', ''))
	assert resp.body == '/foo?'
	resp2 := h.handle(new_request(.get, '/other', ''))
	assert resp2.body == '/other?'
}
