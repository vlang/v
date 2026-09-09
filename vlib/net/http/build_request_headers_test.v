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

fn test_build_request_headers_trims_repeated_fields_before_combining() {
	mut req := Request{}
	req.header.add_custom('X-Foo', ' a ')!
	req.header.add_custom('X-Foo', ' b ')!
	headers := req.build_request_headers(.get, 'localhost', 80, '/')
	assert headers.contains('X-Foo: a, b\r\n')
}

fn test_build_request_headers_semicolon_combines_case_insensitive_cookie_fields() {
	mut req := Request{}
	req.header.add_custom('cookie', 'a=1')!
	req.header.add_custom('cookie', 'b=2')!
	headers := req.build_request_headers(.get, 'localhost', 80, '/')
	assert headers.count('Cookie:') == 1
	assert headers.contains('Cookie: a=1; b=2\r\n')
}

fn test_build_request_headers_preserves_present_empty_cookie_field() {
	mut req := Request{}
	req.header.add_custom('Cookie', '')!
	headers := req.build_request_headers(.get, 'localhost', 80, '/')
	assert headers.contains('Cookie: \r\n')
}

fn test_build_request_headers_deduplicates_header_name_casing() {
	mut req := Request{}
	req.header.add_custom('X-Foo', 'a')!
	req.header.add_custom('x-foo', 'b')!
	headers := req.build_request_headers(.get, 'localhost', 80, '/')
	assert headers.count('X-Foo:') == 1
	assert headers.contains('X-Foo: a, b\r\n')
}

fn test_build_request_headers_preserves_nondefault_port_for_scheme() {
	req := Request{}
	http_headers := req.build_request_headers_with(.get, 'example.com', 443, 80, '/', '',
		new_header())
	assert http_headers.contains('Host: example.com:443\r\n')
	https_headers := req.build_request_headers_with(.get, 'example.com', 80, 443, '/', '',
		new_header())
	assert https_headers.contains('Host: example.com:80\r\n')
}

fn test_build_request_headers_brackets_ipv6_authority() {
	req := Request{}
	headers := req.build_request_headers_with(.get, '2001:db8::1', 443, 443, '/', '', new_header())
	assert headers.contains('Host: [2001:db8::1]\r\n')
}
