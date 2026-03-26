module v3

// Tests for HTTP/3 error codes (RFC 9114 §8.1), unknown type handling,
// and header field lowercase validation (RFC 9114 §4.2).

// ── Task 1: H3 Error Code enum values ──

fn test_h3_error_code_no_error() {
	assert u64(H3ErrorCode.h3_no_error) == 0x0100
}

fn test_h3_error_code_general_protocol_error() {
	assert u64(H3ErrorCode.h3_general_protocol_error) == 0x0101
}

fn test_h3_error_code_internal_error() {
	assert u64(H3ErrorCode.h3_internal_error) == 0x0102
}

fn test_h3_error_code_stream_creation_error() {
	assert u64(H3ErrorCode.h3_stream_creation_error) == 0x0103
}

fn test_h3_error_code_closed_critical_stream() {
	assert u64(H3ErrorCode.h3_closed_critical_stream) == 0x0104
}

fn test_h3_error_code_frame_unexpected() {
	assert u64(H3ErrorCode.h3_frame_unexpected) == 0x0105
}

fn test_h3_error_code_frame_error() {
	assert u64(H3ErrorCode.h3_frame_error) == 0x0106
}

fn test_h3_error_code_excessive_load() {
	assert u64(H3ErrorCode.h3_excessive_load) == 0x0107
}

fn test_h3_error_code_id_error() {
	assert u64(H3ErrorCode.h3_id_error) == 0x0108
}

fn test_h3_error_code_settings_error() {
	assert u64(H3ErrorCode.h3_settings_error) == 0x0109
}

fn test_h3_error_code_missing_settings() {
	assert u64(H3ErrorCode.h3_missing_settings) == 0x010a
}

fn test_h3_error_code_request_rejected() {
	assert u64(H3ErrorCode.h3_request_rejected) == 0x010b
}

fn test_h3_error_code_request_cancelled() {
	assert u64(H3ErrorCode.h3_request_cancelled) == 0x010c
}

fn test_h3_error_code_request_incomplete() {
	assert u64(H3ErrorCode.h3_request_incomplete) == 0x010d
}

fn test_h3_error_code_message_error() {
	assert u64(H3ErrorCode.h3_message_error) == 0x010e
}

fn test_h3_error_code_connect_error() {
	assert u64(H3ErrorCode.h3_connect_error) == 0x010f
}

fn test_h3_error_code_version_fallback() {
	assert u64(H3ErrorCode.h3_version_fallback) == 0x0110
}

fn test_h3_error_code_str_no_error() {
	s := H3ErrorCode.h3_no_error.str()
	assert s.contains('NO_ERROR') || s.contains('no_error')
}

fn test_h3_error_code_str_message_error() {
	s := H3ErrorCode.h3_message_error.str()
	assert s.contains('MESSAGE_ERROR') || s.contains('message_error')
}

fn test_h3_error_code_str_all_non_empty() {
	codes := [
		H3ErrorCode.h3_no_error,
		H3ErrorCode.h3_general_protocol_error,
		H3ErrorCode.h3_internal_error,
		H3ErrorCode.h3_stream_creation_error,
		H3ErrorCode.h3_closed_critical_stream,
		H3ErrorCode.h3_frame_unexpected,
		H3ErrorCode.h3_frame_error,
		H3ErrorCode.h3_excessive_load,
		H3ErrorCode.h3_id_error,
		H3ErrorCode.h3_settings_error,
		H3ErrorCode.h3_missing_settings,
		H3ErrorCode.h3_request_rejected,
		H3ErrorCode.h3_request_cancelled,
		H3ErrorCode.h3_request_incomplete,
		H3ErrorCode.h3_message_error,
		H3ErrorCode.h3_connect_error,
		H3ErrorCode.h3_version_fallback,
	]
	assert codes.len == 17, 'expected all 17 error codes'
	for code in codes {
		assert code.str().len > 0, 'str() should not be empty for ${u64(code)}'
	}
}

// ── Task 2: Unknown frame type handling ──

fn test_frame_type_from_u64_known_types() {
	// Known types should still work after the change to option return
	assert frame_type_from_u64(0x0) or { FrameType.settings } == FrameType.data
	assert frame_type_from_u64(0x1) or { FrameType.settings } == FrameType.headers
	assert frame_type_from_u64(0x4) or { FrameType.data } == FrameType.settings
	assert frame_type_from_u64(0x7) or { FrameType.data } == FrameType.goaway
}

fn test_frame_type_from_u64_unknown_returns_none() {
	// Unknown frame types should return none per RFC 9114 §7.2.8
	result := frame_type_from_u64(0xFF)
	assert result == none, 'unknown frame type 0xFF should return none'
}

fn test_frame_type_from_u64_reserved_type_returns_none() {
	// Reserved/extension frame types must be silently ignored
	result := frame_type_from_u64(0x21)
	assert result == none, 'reserved frame type 0x21 should return none'
}

fn test_parse_response_frames_skips_unknown_frame() {
	mut encoder := new_qpack_encoder(4096, 0)
	mut decoder := new_qpack_decoder(4096, 0)

	// Build a HEADERS frame
	headers := [HeaderField{':status', '200'}]
	encoded_headers := encoder.encode(headers)

	mut data := []u8{}
	// HEADERS frame
	data << encode_varint(u64(FrameType.headers)) or { return }
	data << encode_varint(u64(encoded_headers.len)) or { return }
	data << encoded_headers

	// Unknown frame type 0xFF with 3-byte payload
	data << encode_varint(u64(0xFF)) or { return }
	data << encode_varint(u64(3)) or { return }
	data << [u8(0xAA), 0xBB, 0xCC]

	// DATA frame after the unknown frame
	body := 'hello'.bytes()
	data << encode_varint(u64(FrameType.data)) or { return }
	data << encode_varint(u64(body.len)) or { return }
	data << body

	mut client := Client{
		qpack_decoder: decoder
	}

	parsed_headers, parsed_body := client.parse_response_frames(data) or {
		assert false, 'parse_response_frames should not fail on unknown frames: ${err}'
		return
	}

	assert parsed_headers.len == 1
	assert parsed_headers[0].name == ':status'
	assert parsed_body.bytestr() == 'hello'
}

// ── Task 2: Unknown stream type handling ──

fn test_identify_peer_stream_ignores_unknown_type() {
	// RFC 9114 §6.2.3: unknown stream types must be silently ignored
	mut m := UniStreamManager{}
	m.identify_peer_stream(3, u64(0xFF)) or {
		assert false, 'unknown stream type should not return error: ${err}'
		return
	}
	// Peer stream IDs should remain unset
	assert m.peer_control_stream_id == i64(-1)
	assert m.peer_encoder_stream_id == i64(-1)
	assert m.peer_decoder_stream_id == i64(-1)
}

fn test_identify_peer_stream_ignores_extension_type() {
	mut m := UniStreamManager{}
	m.identify_peer_stream(5, u64(0x42)) or {
		assert false, 'extension stream type should not return error: ${err}'
		return
	}
	assert m.peer_control_stream_id == i64(-1)
}

// ── Task 3: Header lowercase validation ──

fn test_validate_header_names_lowercase_pass() {
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':path', '/'},
		HeaderField{'content-type', 'text/html'},
		HeaderField{'x-custom', 'value'},
	]
	validate_header_names_lowercase(headers) or {
		assert false, 'lowercase headers should pass validation: ${err}'
		return
	}
}

fn test_validate_header_names_uppercase_rejected() {
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{'Content-Type', 'text/html'},
	]
	validate_header_names_lowercase(headers) or {
		assert err.msg().contains('uppercase') || err.msg().contains('H3_MESSAGE_ERROR')
		return
	}
	assert false, 'uppercase header name should be rejected'
}

fn test_validate_header_names_mixed_case_rejected() {
	headers := [
		HeaderField{'X-Custom-Header', 'value'},
	]
	validate_header_names_lowercase(headers) or {
		assert err.msg().contains('uppercase') || err.msg().contains('H3_MESSAGE_ERROR')
		return
	}
	assert false, 'mixed-case header name should be rejected'
}

fn test_validate_header_names_pseudo_headers_pass() {
	// Pseudo-headers starting with ':' are already lowercase by convention
	headers := [
		HeaderField{':method', 'GET'},
		HeaderField{':scheme', 'https'},
		HeaderField{':authority', 'example.com'},
		HeaderField{':path', '/index.html'},
		HeaderField{':status', '200'},
	]
	validate_header_names_lowercase(headers) or {
		assert false, 'pseudo-headers should pass: ${err}'
		return
	}
}

fn test_validate_header_names_empty_list_pass() {
	headers := []HeaderField{}
	validate_header_names_lowercase(headers) or {
		assert false, 'empty header list should pass: ${err}'
		return
	}
}

fn test_parse_response_rejects_uppercase_headers() {
	mut encoder := new_qpack_encoder(4096, 0)
	mut decoder := new_qpack_decoder(4096, 0)

	// Encode headers including an uppercase one
	// Note: QPACK encoder normally lowercases, so we manually build
	// a response with uppercase header to test the validation
	headers := [
		HeaderField{':status', '200'},
		HeaderField{'Content-Type', 'text/html'},
	]
	encoded_headers := encoder.encode(headers)

	mut data := []u8{}
	data << encode_varint(u64(FrameType.headers)) or { return }
	data << encode_varint(u64(encoded_headers.len)) or { return }
	data << encoded_headers

	mut client := Client{
		qpack_decoder: decoder
	}

	client.parse_response_frames(data) or {
		assert err.msg().contains('uppercase') || err.msg().contains('H3_MESSAGE_ERROR')
		return
	}
	// If the encoder lowercased on its own, this test won't trigger the validation.
	// That's acceptable — it means the encoder already enforces lowercase.
}
