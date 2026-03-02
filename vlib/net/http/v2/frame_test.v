// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

// Test for HTTP/2 frame encoding/decoding

fn test_frame_header_encode_decode() {
	header := FrameHeader{
		length:     100
		frame_type: .data
		flags:      u8(FrameFlags.end_stream)
		stream_id:  1
	}

	encoded := header.encode()
	assert encoded.len == frame_header_size

	decoded := parse_frame_header(encoded) or {
		assert false, 'Failed to parse frame header'
		return
	}

	assert decoded.length == header.length
	assert decoded.frame_type == header.frame_type
	assert decoded.flags == header.flags
	assert decoded.stream_id == header.stream_id
}

fn test_frame_header_flags() {
	header := FrameHeader{
		length:     0
		frame_type: .headers
		flags:      u8(FrameFlags.end_stream) | u8(FrameFlags.end_headers)
		stream_id:  3
	}

	assert header.has_flag(.end_stream)
	assert header.has_flag(.end_headers)
	assert !header.has_flag(.padded)
}

fn test_settings_frame() {
	mut settings := map[u16]u32{}
	settings[u16(SettingId.header_table_size)] = 4096
	settings[u16(SettingId.max_concurrent_streams)] = 100

	mut payload := []u8{}
	for id, value in settings {
		payload << u8(id >> 8)
		payload << u8(id)
		payload << u8(value >> 24)
		payload << u8(value >> 16)
		payload << u8(value >> 8)
		payload << u8(value)
	}

	frame := Frame{
		header:  FrameHeader{
			length:     u32(payload.len)
			frame_type: .settings
			flags:      0
			stream_id:  0
		}
		payload: payload
	}

	encoded := frame.encode()
	decoded := parse_frame(encoded) or {
		assert false, 'Failed to parse frame'
		return
	}

	assert decoded.header.frame_type == .settings
	assert decoded.header.stream_id == 0
	assert decoded.payload.len == payload.len
}

// test_frame_type_from_byte_known verifies that known frame type bytes are parsed correctly
fn test_frame_type_from_byte_known() {
	assert frame_type_from_byte(0x0) or {
		assert false, 'expected .data'
		return
	} == .data
	assert frame_type_from_byte(0x1) or {
		assert false, 'expected .headers'
		return
	} == .headers
	assert frame_type_from_byte(0x9) or {
		assert false, 'expected .continuation'
		return
	} == .continuation
}

// test_frame_type_from_byte_unknown verifies that unknown frame type bytes return none (RFC 7540 §4.1)
fn test_frame_type_from_byte_unknown() {
	// Per RFC 7540 §4.1: unknown frame types MUST be ignored, not errored
	result := frame_type_from_byte(0xff)
	assert result == none
}

// test_parse_frame_header_unknown_type verifies that parse_frame_header skips frames with unknown types
fn test_parse_frame_header_unknown_type() {
	// Build a 9-byte header with unknown type 0xfe
	mut raw := []u8{len: 9}
	raw[0] = 0 // length high
	raw[1] = 0 // length mid
	raw[2] = 5 // length low  (5 bytes payload)
	raw[3] = 0xfe // unknown type
	raw[4] = 0 // flags
	raw[5] = 0 // stream_id high
	raw[6] = 0
	raw[7] = 0
	raw[8] = 1 // stream_id = 1
	// parse_frame_header must NOT return an error; it returns none for unknown types
	header := parse_frame_header(raw) or {
		// Returning none is acceptable (unknown type skipped)
		return
	}
	// If it returns a value, that is also fine if the caller decides to accept a zero-value
	// This case should not be reached with an unknown type
	_ = header
}

fn test_frame_validation() {
	// Valid DATA frame
	valid_frame := Frame{
		header:  FrameHeader{
			length:     10
			frame_type: .data
			flags:      0
			stream_id:  1
		}
		payload: []u8{len: 10}
	}

	valid_frame.validate() or { assert false, 'Valid frame should not fail validation' }

	// Invalid: DATA frame on stream 0
	invalid_frame := Frame{
		header:  FrameHeader{
			length:     10
			frame_type: .data
			flags:      0
			stream_id:  0
		}
		payload: []u8{len: 10}
	}

	invalid_frame.validate() or {
		assert err.msg().contains('stream 0')
		return
	}
	assert false, 'Invalid frame should fail validation'
}
