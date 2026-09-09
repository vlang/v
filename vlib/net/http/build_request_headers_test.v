module http

fn test_build_request_headers_with_empty_body_adds_content_length_zero() {
	// Create a request with no data.
	mut req := Request{}
	// Build the headers for it. Ensure that Content-Length: 0 is added
	// for requests without a body, which is required by some servers.
	// We use a POST request, as it is most likely to be affected by this.
	headers := req.build_request_headers(.post, 'localhost', 80, '/')
	assert headers.contains('Content-Length: 0\r\n')
}

fn test_build_request_headers_comma_combines_repeated_fields() {
	mut req := Request{}
	req.header.add_custom('Accept', 'text/html')!
	req.header.add_custom('Accept', 'application/json')!
	headers := req.build_request_headers(.get, 'localhost', 80, '/')
	assert headers.contains('Accept: text/html, application/json\r\n')
}

fn test_build_request_headers_semicolon_combines_case_insensitive_cookie_fields() {
	mut req := Request{}
	req.header.add_custom('cookie', 'a=1')!
	req.header.add_custom('cookie', 'b=2')!
	headers := req.build_request_headers(.get, 'localhost', 80, '/')
	assert headers.count('Cookie:') == 1
	assert headers.contains('Cookie: a=1; b=2\r\n')
}
