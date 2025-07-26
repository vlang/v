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
