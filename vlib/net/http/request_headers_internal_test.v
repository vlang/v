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
	headers := req.build_request_headers(.post, 'example.com', 80, '/')
	lower := headers.to_lower()
	assert lower.count('host: ') == 1
	assert lower.count('user-agent: ') == 1
	assert lower.count('content-length: ') == 1
	assert lower.contains('host: example.com')
	assert lower.contains('user-agent: already-present')
	assert lower.contains('content-length: 999')
}
