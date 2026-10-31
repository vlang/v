module http

fn test_reverse_proxy_director() {
	p := new_reverse_proxy('http://localhost:8080')
	mut req := new_request(.get, '/foo', '')
	p.director(mut req)
	assert req.url == 'http://localhost:8080/foo'
	assert req.host == 'localhost:8080'
}

fn test_reverse_proxy_new() {
	p := new_reverse_proxy('http://example.com')
	assert p.target == 'http://example.com'
}
