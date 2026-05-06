module v3

// Tests for HTTP/3 CONNECT method tunneling per RFC 9114 §4.4.

fn test_h3_connect_request_headers() {
	// CONNECT sends only :method + :authority per RFC 9114 §4.4
	req := ConnectRequest{
		authority: 'proxy.example.com:443'
	}
	headers := build_connect_headers(req)
	mut has_method := false
	mut has_authority := false
	for h in headers {
		if h.name == ':method' {
			assert h.value == 'CONNECT'
			has_method = true
		}
		if h.name == ':authority' {
			assert h.value == 'proxy.example.com:443'
			has_authority = true
		}
	}
	assert has_method, 'CONNECT must have :method pseudo-header'
	assert has_authority, 'CONNECT must have :authority pseudo-header'
}

fn test_h3_connect_no_scheme_no_path() {
	// CONNECT MUST NOT include :scheme or :path per RFC 9114 §4.4
	req := ConnectRequest{
		authority: 'proxy.example.com:443'
	}
	headers := build_connect_headers(req)
	for h in headers {
		assert h.name != ':scheme', ':scheme must not be present in CONNECT request'
		assert h.name != ':path', ':path must not be present in CONNECT request'
	}
}

fn test_h3_connect_tunnel_struct() {
	// ConnectTunnel should be initialized with correct stream_id and open state
	tunnel := ConnectTunnel{
		stream_id: 3
		open:      true
	}
	assert tunnel.stream_id == 3
	assert tunnel.open == true
}

fn test_h3_connect_validation_allows_connect() {
	// Validation should accept CONNECT without :path per RFC 9114 §4.4
	headers := [
		HeaderField{':method', 'CONNECT'},
		HeaderField{':authority', 'proxy.example.com:443'},
	]
	validate_h3_request_headers(headers) or {
		assert false, 'CONNECT without :path should pass validation: ${err}'
		return
	}
}
