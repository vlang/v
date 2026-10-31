module http

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
