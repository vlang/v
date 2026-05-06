module http

import net.urllib

fn test_normalized_origin_uses_effective_port() {
	url := urllib.parse('https://example.com/path') or {
		assert false, err.msg()
		return
	}
	assert normalized_origin(url) == 'https://example.com:443'
}

fn test_build_client_header_does_not_mutate_request_header() {
	mut req := Request{
		url:        'https://example.com'
		data:       'payload'
		user_agent: 'test-agent'
	}
	assert !req.header.contains_custom('user-agent')
	assert !req.header.contains(.content_length)

	built := req.build_client_header()

	assert built.get_custom('user-agent') or { '' } == 'test-agent'
	assert built.get(.content_length) or { '' } == '7'
	assert !req.header.contains_custom('user-agent')
	assert !req.header.contains(.content_length)
}
