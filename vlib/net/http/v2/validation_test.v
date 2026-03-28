module v2

// Tests for RFC 7540 compliance: header validation, unknown frame handling,
// and connection-specific header filtering.

// --- Task 1: Unknown frame type handling (RFC 7540 §5.5) ---

fn test_unknown_frame_type_ignored_by_server_dispatch() {
	// RFC 7540 §5.5: unknown frame types MUST be ignored and discarded.
	// dispatch_frame with an unknown frame type should return without error.
	mut s := Server{
		config: ServerConfig{}
	}
	mut conn := &MockServerConn{}
	mut ctx := ConnContext{
		encoder: new_encoder()
	}
	mut state := LoopState{
		decoder: new_decoder()
	}

	// Frame with type byte 0xFE (unknown) — we simulate by casting directly
	unknown_frame := Frame{
		header:  FrameHeader{
			length:     5
			frame_type: unsafe { FrameType(0xfe) }
			flags:      0
			stream_id:  1
		}
		payload: []u8{len: 5}
	}

	// Should NOT return an error (silently ignored)
	s.dispatch_frame(unknown_frame, mut conn, mut ctx, mut state) or {
		assert false, 'dispatch_frame should silently ignore unknown frame types, got error: ${err}'
		return
	}
	// Should NOT have sent any GOAWAY frame
	assert conn.written_data.len == 0, 'no GOAWAY should be sent for unknown frame types'
}

fn test_unknown_frame_type_ignored_by_client() {
	// Client's handle_response_frame should silently ignore unknown frame types.
	mut c := create_mock_client()

	unknown_frame := Frame{
		header:  FrameHeader{
			length:     3
			frame_type: unsafe { FrameType(0xfe) }
			flags:      0
			stream_id:  1
		}
		payload: []u8{len: 3}
	}

	mut stream := Stream{
		id:    1
		state: .open
	}
	// Should not error
	c.handle_response_frame(unknown_frame, mut stream, 1) or {
		assert false, 'handle_response_frame should ignore unknown frame types, got: ${err}'
		return
	}
}

fn test_read_frame_from_skips_unknown_type() {
	// When read_frame_from encounters an unknown frame type byte,
	// it should read and discard the payload, then continue to the next frame.
	// Build wire bytes: unknown frame (type 0xAB, length 3, stream 1) followed by
	// a valid PING frame.
	mut wire := []u8{}

	// Unknown frame: length=3, type=0xAB, flags=0, stream_id=1
	wire << [u8(0x00), 0x00, 0x03] // length = 3
	wire << u8(0xab) // unknown type
	wire << u8(0x00) // flags
	wire << [u8(0x00), 0x00, 0x00, 0x01] // stream_id = 1
	wire << [u8(0xDE), 0xAD, 0xBE] // 3 bytes payload (to be discarded)

	// Valid PING frame: length=8, type=6, flags=0, stream_id=0
	wire << [u8(0x00), 0x00, 0x08] // length = 8
	wire << u8(0x06) // type = PING
	wire << u8(0x00) // flags
	wire << [u8(0x00), 0x00, 0x00, 0x00] // stream_id = 0
	wire << [u8(1), 2, 3, 4, 5, 6, 7, 8] // 8 bytes payload

	mut conn := &MockServerConn{
		read_data: wire
	}
	frame := read_frame_from(mut conn, 16384) or {
		assert false, 'read_frame_from should skip unknown frame and return the next valid one, got: ${err}'
		return
	}
	assert frame.header.frame_type == .ping, 'expected PING frame after skipping unknown, got ${frame.header.frame_type}'
	assert frame.payload.len == 8
}

// --- Task 2: Malformed request detection (RFC 7540 §8.1.2.6) ---

fn test_validate_missing_method() {
	headers := [
		HeaderField{
			name:  ':path'
			value: '/'
		},
		HeaderField{
			name:  ':scheme'
			value: 'https'
		},
	]
	validate_request_headers(headers) or {
		assert err.msg().contains(':method')
		return
	}
	assert false, 'should reject headers missing :method'
}

fn test_validate_missing_path() {
	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  ':scheme'
			value: 'https'
		},
	]
	validate_request_headers(headers) or {
		assert err.msg().contains(':path')
		return
	}
	assert false, 'should reject headers missing :path'
}

fn test_validate_missing_scheme() {
	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  ':path'
			value: '/'
		},
	]
	validate_request_headers(headers) or {
		assert err.msg().contains(':scheme')
		return
	}
	assert false, 'should reject headers missing :scheme'
}

fn test_validate_unknown_method() {
	headers := [
		HeaderField{
			name:  ':method'
			value: 'BREW'
		},
		HeaderField{
			name:  ':path'
			value: '/'
		},
		HeaderField{
			name:  ':scheme'
			value: 'https'
		},
	]
	validate_request_headers(headers) or {
		assert err.msg().contains('unsupported :method')
		return
	}
	assert false, 'should reject unsupported :method'
}

fn test_validate_unknown_pseudo_header() {
	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  ':path'
			value: '/'
		},
		HeaderField{
			name:  ':scheme'
			value: 'https'
		},
		HeaderField{
			name:  ':unknown'
			value: 'value'
		},
	]
	validate_request_headers(headers) or {
		assert err.msg().contains('pseudo-header')
		return
	}
	assert false, 'should reject unknown pseudo-header :unknown'
}

fn test_validate_pseudo_header_after_regular() {
	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  'content-type'
			value: 'text/html'
		},
		HeaderField{
			name:  ':path'
			value: '/'
		},
	]
	validate_request_headers(headers) or {
		assert err.msg().contains('pseudo-header')
		return
	}
	assert false, 'should reject pseudo-header appearing after regular header'
}

fn test_validate_valid_headers() {
	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  ':path'
			value: '/'
		},
		HeaderField{
			name:  ':scheme'
			value: 'https'
		},
		HeaderField{
			name:  ':authority'
			value: 'example.com'
		},
		HeaderField{
			name:  'content-type'
			value: 'text/html'
		},
		HeaderField{
			name:  'accept'
			value: '*/*'
		},
	]
	validate_request_headers(headers) or {
		assert false, 'valid headers should pass validation, got: ${err}'
		return
	}
}

// --- Task 3: Connection-specific header filtering (RFC 7540 §8.1.2.2) ---

fn test_validate_connection_header_rejected() {
	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  ':path'
			value: '/'
		},
		HeaderField{
			name:  ':scheme'
			value: 'https'
		},
		HeaderField{
			name:  'connection'
			value: 'keep-alive'
		},
	]
	validate_request_headers(headers) or {
		assert err.msg().contains('connection')
		return
	}
	assert false, 'should reject connection header in HTTP/2'
}

fn test_validate_keep_alive_header_rejected() {
	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  ':path'
			value: '/'
		},
		HeaderField{
			name:  ':scheme'
			value: 'https'
		},
		HeaderField{
			name:  'keep-alive'
			value: 'timeout=5'
		},
	]
	validate_request_headers(headers) or {
		assert err.msg().contains('keep-alive')
		return
	}
	assert false, 'should reject keep-alive header in HTTP/2'
}

fn test_validate_transfer_encoding_chunked_rejected() {
	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  ':path'
			value: '/'
		},
		HeaderField{
			name:  ':scheme'
			value: 'https'
		},
		HeaderField{
			name:  'transfer-encoding'
			value: 'chunked'
		},
	]
	validate_request_headers(headers) or {
		assert err.msg().contains('transfer-encoding')
		return
	}
	assert false, 'should reject transfer-encoding: chunked in HTTP/2'
}

fn test_validate_transfer_encoding_trailers_allowed() {
	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  ':path'
			value: '/'
		},
		HeaderField{
			name:  ':scheme'
			value: 'https'
		},
		HeaderField{
			name:  'transfer-encoding'
			value: 'trailers'
		},
	]
	validate_request_headers(headers) or {
		assert false, 'transfer-encoding: trailers should be allowed, got: ${err}'
		return
	}
}

fn test_validate_upgrade_header_rejected() {
	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  ':path'
			value: '/'
		},
		HeaderField{
			name:  ':scheme'
			value: 'https'
		},
		HeaderField{
			name:  'upgrade'
			value: 'websocket'
		},
	]
	validate_request_headers(headers) or {
		assert err.msg().contains('upgrade')
		return
	}
	assert false, 'should reject upgrade header in HTTP/2'
}

fn test_validate_proxy_connection_rejected() {
	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  ':path'
			value: '/'
		},
		HeaderField{
			name:  ':scheme'
			value: 'https'
		},
		HeaderField{
			name:  'proxy-connection'
			value: 'keep-alive'
		},
	]
	validate_request_headers(headers) or {
		assert err.msg().contains('proxy-connection')
		return
	}
	assert false, 'should reject proxy-connection header in HTTP/2'
}

fn test_validate_regular_headers_pass() {
	headers := [
		HeaderField{
			name:  ':method'
			value: 'POST'
		},
		HeaderField{
			name:  ':path'
			value: '/api/data'
		},
		HeaderField{
			name:  ':scheme'
			value: 'https'
		},
		HeaderField{
			name:  'content-type'
			value: 'application/json'
		},
		HeaderField{
			name:  'accept'
			value: 'application/json'
		},
		HeaderField{
			name:  'x-custom'
			value: 'value'
		},
	]
	validate_request_headers(headers) or {
		assert false, 'regular headers should pass validation, got: ${err}'
		return
	}
}

fn test_client_filters_connection_specific_headers() {
	// filter_connection_specific_headers should remove forbidden headers
	// but keep allowed ones.
	input := {
		'content-type':      'text/html'
		'connection':        'keep-alive'
		'keep-alive':        'timeout=5'
		'proxy-connection':  'keep-alive'
		'transfer-encoding': 'chunked'
		'upgrade':           'websocket'
		'accept':            '*/*'
	}
	result := filter_connection_specific_headers(input)
	assert 'content-type' in result
	assert 'accept' in result
	assert 'connection' !in result
	assert 'keep-alive' !in result
	assert 'proxy-connection' !in result
	assert 'transfer-encoding' !in result
	assert 'upgrade' !in result
}

fn test_client_filter_keeps_te_trailers() {
	// transfer-encoding: trailers is the only allowed TE value
	input := {
		'transfer-encoding': 'trailers'
		'content-type':      'text/html'
	}
	result := filter_connection_specific_headers(input)
	assert 'transfer-encoding' in result
	assert 'content-type' in result
}

// --- Task P2-4: Response pseudo-header validation (RFC 7540 §8.1.2.1/§8.1.2.3) ---

fn test_validate_response_missing_status() {
	// Response MUST contain :status pseudo-header.
	headers := [
		HeaderField{
			name:  'content-type'
			value: 'text/html'
		},
	]
	validate_response_headers(headers) or {
		assert err.msg().contains(':status')
		return
	}
	assert false, 'should reject response missing :status'
}

fn test_validate_response_with_method_pseudo_header() {
	// Response MUST NOT contain request pseudo-headers like :method.
	headers := [
		HeaderField{
			name:  ':status'
			value: '200'
		},
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
	]
	validate_response_headers(headers) or {
		assert err.msg().contains(':method')
		return
	}
	assert false, 'should reject response containing :method'
}

fn test_validate_response_with_path_pseudo_header() {
	headers := [
		HeaderField{
			name:  ':status'
			value: '200'
		},
		HeaderField{
			name:  ':path'
			value: '/'
		},
	]
	validate_response_headers(headers) or {
		assert err.msg().contains(':path')
		return
	}
	assert false, 'should reject response containing :path'
}

fn test_validate_response_with_scheme_pseudo_header() {
	headers := [
		HeaderField{
			name:  ':status'
			value: '200'
		},
		HeaderField{
			name:  ':scheme'
			value: 'https'
		},
	]
	validate_response_headers(headers) or {
		assert err.msg().contains(':scheme')
		return
	}
	assert false, 'should reject response containing :scheme'
}

fn test_validate_response_pseudo_after_regular() {
	// Pseudo-headers must come before regular headers.
	headers := [
		HeaderField{
			name:  'content-type'
			value: 'text/html'
		},
		HeaderField{
			name:  ':status'
			value: '200'
		},
	]
	validate_response_headers(headers) or {
		assert err.msg().contains('pseudo-header')
		return
	}
	assert false, 'should reject pseudo-header after regular header'
}

fn test_validate_response_valid() {
	headers := [
		HeaderField{
			name:  ':status'
			value: '200'
		},
		HeaderField{
			name:  'content-type'
			value: 'text/html'
		},
		HeaderField{
			name:  'content-length'
			value: '42'
		},
	]
	validate_response_headers(headers) or {
		assert false, 'valid response should pass validation, got: ${err}'
		return
	}
}

fn test_validate_response_valid_no_body_headers() {
	// Minimal valid response: just :status
	headers := [
		HeaderField{
			name:  ':status'
			value: '404'
		},
	]
	validate_response_headers(headers) or {
		assert false, 'response with only :status should pass validation, got: ${err}'
		return
	}
}

// --- Test helpers ---

struct MockServerConn {
mut:
	read_data    []u8
	read_pos     int
	written_data []u8
}

fn (mut m MockServerConn) read(mut buf []u8) !int {
	if m.read_pos >= m.read_data.len {
		return error('EOF')
	}
	n := if m.read_pos + buf.len > m.read_data.len {
		m.read_data.len - m.read_pos
	} else {
		buf.len
	}
	for i in 0 .. n {
		buf[i] = m.read_data[m.read_pos + i]
	}
	m.read_pos += n
	return n
}

fn (mut m MockServerConn) write(data []u8) !int {
	m.written_data << data
	return data.len
}

fn (mut m MockServerConn) close() ! {
}

fn create_mock_client() Client {
	return Client{}
}
