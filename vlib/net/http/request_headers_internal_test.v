module http

fn test_build_request_headers_respects_case_insensitive_existing_headers() {
	req := Request{
		method:     .post
		url:        'http://example.com'
		data:       'hello'
		user_agent: 'custom-agent'
		header: new_custom_header_from_map({
			'host':           'example.com'
			'user-agent':     'already-present'
			'content-length': '999'
		}) or {
			assert false, err.msg()
			return
		}
	}
	headers := req.build_request_headers(.post, 'example.com', 80, '/', req.data)
	lower := headers.to_lower()
	assert lower.count('host: ') == 1
	assert lower.count('user-agent: ') == 1
	assert lower.count('content-length: ') == 1
	assert lower.contains('host: example.com')
	assert lower.contains('user-agent: already-present')
	assert lower.contains('content-length: 999')
}

fn test_build_request_headers_method_override_for_redirect() {
	// Verifies that build_request_headers uses the method parameter (not req.method).
	// This is critical for 303 redirect handling where do() passes .get
	// even though req.method is .post.
	req := Request{
		method:     .post
		url:        'http://example.com'
		data:       'post_body'
		user_agent: 'v.http'
	}
	// When method parameter is GET (simulating 303 redirect), headers should show GET
	headers_get := req.build_request_headers(.get, 'example.com', 80, '/redirected', '')
	assert headers_get.starts_with('GET /redirected HTTP/1.1\r\n')
	// When method parameter is POST (original or 307/308), headers should show POST
	headers_post := req.build_request_headers(.post, 'example.com', 80, '/original', req.data)
	assert headers_post.starts_with('POST /original HTTP/1.1\r\n')
}
