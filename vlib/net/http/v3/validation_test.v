module v3

// Tests for HTTP/3 request header validation per RFC 9114 §4.1.2.

// new_validation_test_conn creates a minimal ServerConnection for validation tests.
fn new_validation_test_conn() ServerConnection {
	return ServerConnection{
		encoder:  new_qpack_encoder(4096, 100)
		decoder:  new_qpack_decoder(4096, 100)
		settings: Settings{
			max_field_section_size:   8192
			qpack_max_table_capacity: 4096
			qpack_blocked_streams:    100
		}
	}
}

fn test_validate_h3_request_headers_missing_method() {
	headers := [
		HeaderField{':path', '/'},
		HeaderField{':scheme', 'https'},
	]
	validate_h3_request_headers(headers) or {
		assert err.msg().contains(':method')
		return
	}
	assert false, 'expected error for missing :method'
}

fn test_validate_h3_request_headers_missing_path() {
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':scheme', 'https'},
	]
	validate_h3_request_headers(headers) or {
		assert err.msg().contains(':path')
		return
	}
	assert false, 'expected error for missing :path'
}

fn test_validate_h3_request_headers_unknown_pseudo_header() {
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':path', '/'},
		HeaderField{':foo', 'bar'},
	]
	validate_h3_request_headers(headers) or {
		assert err.msg().contains(':foo')
		return
	}
	assert false, 'expected error for unknown pseudo-header :foo'
}

fn test_validate_h3_request_headers_pseudo_after_regular() {
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{'content-type', 'text/html'},
		HeaderField{':path', '/'},
	]
	validate_h3_request_headers(headers) or {
		assert err.msg().contains(':path') || err.msg().contains('pseudo-header')
		return
	}
	assert false, 'expected error for pseudo-header after regular header'
}

fn test_validate_h3_request_headers_connection_specific_header() {
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':path', '/'},
		HeaderField{'connection', 'keep-alive'},
	]
	validate_h3_request_headers(headers) or {
		assert err.msg().contains('connection')
		return
	}
	assert false, 'expected error for connection-specific header'
}

fn test_validate_h3_request_headers_keep_alive_forbidden() {
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':path', '/'},
		HeaderField{'keep-alive', 'timeout=5'},
	]
	validate_h3_request_headers(headers) or {
		assert err.msg().contains('keep-alive')
		return
	}
	assert false, 'expected error for keep-alive header'
}

fn test_validate_h3_request_headers_transfer_encoding_forbidden() {
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':path', '/'},
		HeaderField{'transfer-encoding', 'chunked'},
	]
	validate_h3_request_headers(headers) or {
		assert err.msg().contains('transfer-encoding')
		return
	}
	assert false, 'expected error for transfer-encoding header'
}

fn test_validate_h3_request_headers_upgrade_forbidden() {
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':path', '/'},
		HeaderField{'upgrade', 'websocket'},
	]
	validate_h3_request_headers(headers) or {
		assert err.msg().contains('upgrade')
		return
	}
	assert false, 'expected error for upgrade header'
}

fn test_validate_h3_request_headers_proxy_connection_forbidden() {
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':path', '/'},
		HeaderField{'proxy-connection', 'keep-alive'},
	]
	validate_h3_request_headers(headers) or {
		assert err.msg().contains('proxy-connection')
		return
	}
	assert false, 'expected error for proxy-connection header'
}

fn test_validate_h3_request_headers_valid_minimal() {
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':path', '/'},
	]
	validate_h3_request_headers(headers) or {
		assert false, 'valid minimal headers should pass: ${err}'
		return
	}
}

fn test_validate_h3_request_headers_valid_full() {
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':path', '/index.html'},
		HeaderField{':scheme', 'https'},
		HeaderField{':authority', 'example.com'},
		HeaderField{'content-type', 'text/html'},
		HeaderField{'accept', '*/*'},
	]
	validate_h3_request_headers(headers) or {
		assert false, 'valid full headers should pass: ${err}'
		return
	}
}

fn test_validate_h3_request_headers_connect_no_path_needed() {
	// CONNECT method does not require :path per RFC 9114 §4.4
	headers := [
		HeaderField{':method', 'CONNECT'},
		HeaderField{':authority', 'proxy.example.com:443'},
	]
	validate_h3_request_headers(headers) or {
		assert false, 'CONNECT without :path should pass: ${err}'
		return
	}
}

fn test_validate_h3_request_headers_protocol_pseudo_header() {
	// :protocol is valid for extended CONNECT (RFC 9220)
	headers := [
		HeaderField{':method', 'CONNECT'},
		HeaderField{':protocol', 'websocket'},
		HeaderField{':path', '/ws'},
		HeaderField{':scheme', 'https'},
		HeaderField{':authority', 'example.com'},
	]
	validate_h3_request_headers(headers) or {
		assert false, ':protocol should be accepted: ${err}'
		return
	}
}

// ── Task 2: MAX_CONCURRENT_STREAMS enforcement ──

fn test_max_concurrent_streams_rejects_excess() {
	mut s := Server{
		config: ServerConfig{
			max_concurrent_streams: 2
		}
	}
	mut conn := new_validation_test_conn()

	// Create 2 streams to reach the limit
	conn.streams[u64(0)] = &ServerStream{
		id:               0
		headers_received: true
	}
	conn.streams[u64(4)] = &ServerStream{
		id:               4
		headers_received: true
	}

	// Encode minimal valid headers for a third stream
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':path', '/'},
	]
	encoded := conn.encoder.encode(headers)

	// Attempt to open a third stream — should be rejected
	s.handle_headers_frame(mut conn, u64(8), encoded) or {
		assert err.msg().contains('MAX_CONCURRENT_STREAMS') || err.msg().contains('concurrent')
			|| err.msg().contains('H3_ID_ERROR')
		return
	}
	assert false, 'expected error when exceeding max_concurrent_streams'
}

fn test_max_concurrent_streams_allows_within_limit() {
	mut s := Server{
		config: ServerConfig{
			max_concurrent_streams: 5
		}
	}
	mut conn := new_validation_test_conn()

	// Create 1 stream — well within limit
	conn.streams[u64(0)] = &ServerStream{
		id:               0
		headers_received: true
		request_complete: true
	}

	// Encode POST headers — POST waits for DATA, so process_request is not called
	headers := [
		HeaderField{':method', 'POST'},
		HeaderField{':path', '/'},
	]
	encoded := conn.encoder.encode(headers)

	// Second stream should succeed (stream 4 is new, not in map yet)
	// Use handle_headers_frame which will create the stream
	// Mark request_complete on existing to prevent process_request needing crypto
	s.handle_headers_frame(mut conn, u64(4), encoded) or {
		assert false, 'should allow stream within limit: ${err}'
		return
	}
}
