module http

fn test_set_server_only_header_overwrites_client_supplied_value_case_insensitively() {
	mut header := new_custom_header_from_map({
		'remote-addr': 'spoofed'
		'Remote-Addr': 'also-spoofed'
	}) or {
		assert false, err.msg()
		return
	}
	set_server_only_header(mut header, 'Remote-Addr', '127.0.0.1')
	assert header.custom_values('remote-addr').len == 1
	assert header.get_custom('remote-addr') or { '' } == '127.0.0.1'
}

// === O3: DebugHandler must not echo sensitive data ===

fn test_debug_handler_does_not_echo_request_body() {
	// O3: DebugHandler must not echo the raw request body in the response,
	// as it could contain sensitive data (credentials, tokens, PII).
	handler := DebugHandler{}
	mut req_header := new_header(key: .authorization, value: 'Bearer secret-token')
	req_header.add(.cookie, 'session=abc123')
	req := ServerRequest{
		method: .post
		path:   '/test'
		body:   'password=hunter2&secret=data'.bytes()
		header: req_header
	}
	resp := handler.handle(req)
	body_str := resp.body.bytestr()

	// Response must NOT contain the raw request body
	assert !body_str.contains('hunter2'), 'DebugHandler must not echo request body containing secrets'
	assert !body_str.contains('password='), 'DebugHandler must not echo request body'

	// Response must NOT echo sensitive headers
	assert !body_str.contains('secret-token'), 'DebugHandler must not echo Authorization header'
	assert !body_str.contains('session=abc123'), 'DebugHandler must not echo Cookie header'

	// Response should contain safe metadata
	assert body_str.contains('POST'), 'response should contain method'
	assert body_str.contains('/test'), 'response should contain path'
	assert resp.status_code == 200, 'status should be 200'
}

fn test_debug_handler_response_contains_safe_metadata() {
	// O3: DebugHandler should return safe metadata (method, path, content-length)
	handler := DebugHandler{}
	req := ServerRequest{
		method: .get
		path:   '/api/health'
		body:   'some body data'.bytes()
	}
	resp := handler.handle(req)
	body_str := resp.body.bytestr()

	assert body_str.contains('GET'), 'response should contain HTTP method'
	assert body_str.contains('/api/health'), 'response should contain request path'
	assert body_str.contains('14'), 'response should contain content length'
}
