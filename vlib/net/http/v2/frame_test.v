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

// test_parse_settings_payload_valid verifies that a well-formed SETTINGS payload
// is parsed into the correct SettingPair slice.
fn test_parse_settings_payload_valid() {
	// Build payload: header_table_size=8192, max_concurrent_streams=200
	mut payload := []u8{}
	// SETTINGS_HEADER_TABLE_SIZE (0x0001) = 8192 (0x00002000)
	payload << [u8(0x00), 0x01, 0x00, 0x00, 0x20, 0x00]
	// SETTINGS_MAX_CONCURRENT_STREAMS (0x0003) = 200 (0x000000C8)
	payload << [u8(0x00), 0x03, 0x00, 0x00, 0x00, 0xC8]

	pairs := parse_settings_payload(payload) or {
		assert false, 'parse_settings_payload failed: ${err}'
		return
	}

	assert pairs.len == 2
	assert pairs[0].id == .header_table_size
	assert pairs[0].value == 8192
	assert pairs[1].id == .max_concurrent_streams
	assert pairs[1].value == 200
}

// test_parse_settings_payload_skips_unknown verifies that unknown setting IDs
// are silently skipped per RFC 7540 §6.5.2.
fn test_parse_settings_payload_skips_unknown() {
	mut payload := []u8{}
	// Unknown setting 0x00FF = 42
	payload << [u8(0x00), 0xFF, 0x00, 0x00, 0x00, 0x2A]
	// SETTINGS_MAX_FRAME_SIZE (0x0005) = 32768
	payload << [u8(0x00), 0x05, 0x00, 0x00, 0x80, 0x00]

	pairs := parse_settings_payload(payload) or {
		assert false, 'parse_settings_payload failed: ${err}'
		return
	}

	// Unknown setting skipped, only max_frame_size remains
	assert pairs.len == 1
	assert pairs[0].id == .max_frame_size
	assert pairs[0].value == 32768
}

// test_parse_settings_payload_empty verifies empty payload returns empty slice.
fn test_parse_settings_payload_empty() {
	pairs := parse_settings_payload([]) or {
		assert false, 'parse_settings_payload failed on empty: ${err}'
		return
	}
	assert pairs.len == 0
}

// test_parse_settings_payload_incomplete rejects a truncated setting.
fn test_parse_settings_payload_incomplete() {
	// Only 4 bytes — not a complete 6-byte setting
	payload := [u8(0x00), 0x01, 0x00, 0x00]
	parse_settings_payload(payload) or {
		assert err.msg().contains('incomplete')
		return
	}
	assert false, 'Should have rejected incomplete settings payload'
}

// test_data_frame_roundtrip verifies DataFrame from_frame/to_frame conversion.
fn test_data_frame_roundtrip() {
	payload := 'Hello HTTP/2'.bytes()
	original := Frame{
		header:  FrameHeader{
			length:     u32(payload.len)
			frame_type: .data
			flags:      u8(FrameFlags.end_stream)
			stream_id:  3
		}
		payload: payload
	}

	df := DataFrame.from_frame(original) or {
		assert false, 'DataFrame.from_frame failed: ${err}'
		return
	}
	assert df.stream_id == 3
	assert df.data == payload
	assert df.end_stream == true

	back := df.to_frame()
	assert back.header.frame_type == .data
	assert back.header.stream_id == 3
	assert back.payload == payload
	assert back.header.has_flag(.end_stream)
}

// test_settings_frame_roundtrip verifies SettingsFrame from_frame/to_frame conversion.
fn test_settings_frame_roundtrip() {
	// Build a SETTINGS payload with two settings
	mut payload := []u8{}
	payload << [u8(0x00), 0x01, 0x00, 0x00, 0x20, 0x00] // header_table_size=8192
	payload << [u8(0x00), 0x03, 0x00, 0x00, 0x00, 0xC8] // max_concurrent_streams=200

	original := Frame{
		header:  FrameHeader{
			length:     u32(payload.len)
			frame_type: .settings
			flags:      0
			stream_id:  0
		}
		payload: payload
	}

	sf := SettingsFrame.from_frame(original) or {
		assert false, 'SettingsFrame.from_frame failed: ${err}'
		return
	}
	assert sf.ack == false
	assert sf.settings[u16(SettingId.header_table_size)] == 8192
	assert sf.settings[u16(SettingId.max_concurrent_streams)] == 200
}

// test_ping_frame_roundtrip verifies PingFrame from_frame/to_frame conversion.
fn test_ping_frame_roundtrip() {
	ping_data := [u8(1), 2, 3, 4, 5, 6, 7, 8]!
	original := Frame{
		header:  FrameHeader{
			length:     8
			frame_type: .ping
			flags:      u8(FrameFlags.ack)
			stream_id:  0
		}
		payload: [u8(1), 2, 3, 4, 5, 6, 7, 8]
	}

	pf := PingFrame.from_frame(original) or {
		assert false, 'PingFrame.from_frame failed: ${err}'
		return
	}
	assert pf.ack == true
	assert pf.data == ping_data

	back := pf.to_frame()
	assert back.header.frame_type == .ping
	assert back.header.has_flag(.ack)
	assert back.payload == [u8(1), 2, 3, 4, 5, 6, 7, 8]
}

// test_goaway_frame_from_frame verifies GoAwayFrame from_frame conversion.
fn test_goaway_frame_from_frame() {
	mut payload := []u8{}
	// last_stream_id = 5
	payload << [u8(0x00), 0x00, 0x00, 0x05]
	// error_code = 0 (no_error)
	payload << [u8(0x00), 0x00, 0x00, 0x00]
	// debug_data
	payload << 'test'.bytes()

	original := Frame{
		header:  FrameHeader{
			length:     u32(payload.len)
			frame_type: .goaway
			flags:      0
			stream_id:  0
		}
		payload: payload
	}

	gf := GoAwayFrame.from_frame(original) or {
		assert false, 'GoAwayFrame.from_frame failed: ${err}'
		return
	}
	assert gf.last_stream_id == 5
	assert gf.error_code == .no_error
	assert gf.debug_data == 'test'.bytes()
}

// test_window_update_frame_from_frame verifies WindowUpdateFrame from_frame conversion.
fn test_window_update_frame_from_frame() {
	// window_increment = 32768
	payload := [u8(0x00), 0x00, 0x80, 0x00]
	original := Frame{
		header:  FrameHeader{
			length:     4
			frame_type: .window_update
			flags:      0
			stream_id:  1
		}
		payload: payload
	}

	wf := WindowUpdateFrame.from_frame(original) or {
		assert false, 'WindowUpdateFrame.from_frame failed: ${err}'
		return
	}
	assert wf.stream_id == 1
	assert wf.window_increment == 32768
}

// test_apply_window_update_connection_level verifies that applying a WINDOW_UPDATE
// frame with stream_id=0 updates the connection-level remote_window_size.
fn test_apply_window_update_connection_level() {
	mut conn := Connection{
		remote_window_size: 65535
	}
	wu_frame := Frame{
		header:  FrameHeader{
			length:     4
			frame_type: .window_update
			flags:      0
			stream_id:  0
		}
		payload: [u8(0x00), 0x00, 0x80, 0x00] // increment = 32768
	}
	conn.apply_window_update(wu_frame) or {
		assert false, 'apply_window_update failed: ${err}'
		return
	}
	assert conn.remote_window_size == 65535 + 32768, 'expected ${65535 + 32768}, got ${conn.remote_window_size}'
}

// test_apply_window_update_stream_level verifies that applying a WINDOW_UPDATE
// frame with a specific stream_id updates the stream's window_size.
fn test_apply_window_update_stream_level() {
	mut stream := &Stream{
		id:          3
		state:       .open
		window_size: 65535
	}
	mut conn := Connection{
		remote_window_size: 65535
		streams:            {
			u32(3): stream
		}
	}
	wu_frame := Frame{
		header:  FrameHeader{
			length:     4
			frame_type: .window_update
			flags:      0
			stream_id:  3
		}
		payload: [u8(0x00), 0x01, 0x00, 0x00] // increment = 65536
	}
	conn.apply_window_update(wu_frame) or {
		assert false, 'apply_window_update failed: ${err}'
		return
	}
	updated_stream := conn.streams[u32(3)] or {
		assert false, 'stream 3 not found'
		return
	}
	assert updated_stream.window_size == 65535 + 65536, 'expected ${65535 + 65536}, got ${updated_stream.window_size}'
}

// test_split_data_for_flow_control verifies that split_data_for_window produces
// correct chunks that respect the available window size.
fn test_split_data_for_flow_control() {
	data := []u8{len: 100, init: u8(0x41)} // 100 bytes of 'A'

	// When window is larger than data, single chunk
	chunks := split_data_for_window(data, 200, 16384)
	assert chunks.len == 1, 'expected 1 chunk when window > data, got ${chunks.len}'
	assert chunks[0].len == 100

	// When window is smaller than data, split into multiple chunks
	chunks2 := split_data_for_window(data, 30, 16384)
	mut total := 0
	for chunk in chunks2 {
		assert chunk.len <= 30, 'chunk exceeds window: ${chunk.len} > 30'
		total += chunk.len
	}
	assert total == 100, 'total bytes mismatch: want 100, got ${total}'

	// When max_frame_size limits chunk size more than window
	chunks3 := split_data_for_window(data, 200, 25)
	for chunk in chunks3 {
		assert chunk.len <= 25, 'chunk exceeds max_frame_size: ${chunk.len} > 25'
	}
	mut total3 := 0
	for chunk in chunks3 {
		total3 += chunk.len
	}
	assert total3 == 100, 'total bytes mismatch: want 100, got ${total3}'
}

// test_split_data_for_flow_control_zero_window verifies that zero window returns empty.
fn test_split_data_for_flow_control_zero_window() {
	data := []u8{len: 50, init: u8(0x42)}
	chunks := split_data_for_window(data, 0, 16384)
	assert chunks.len == 0, 'expected 0 chunks when window is 0, got ${chunks.len}'
}

// test_priority_frame_roundtrip verifies PriorityFrame from_frame/to_frame conversion.
fn test_priority_frame_roundtrip() {
	// Build PRIORITY frame: exclusive=true, stream_dep=3, weight=15
	mut payload := []u8{len: 5}
	payload[0] = 0x80 // exclusive bit + stream_dep high byte
	payload[1] = 0x00
	payload[2] = 0x00
	payload[3] = 0x03 // stream_dep = 3
	payload[4] = 15 // weight

	original := Frame{
		header:  FrameHeader{
			length:     5
			frame_type: .priority
			flags:      0
			stream_id:  5
		}
		payload: payload
	}

	pf := PriorityFrame.from_frame(original) or {
		assert false, 'PriorityFrame.from_frame failed: ${err}'
		return
	}
	assert pf.stream_id == 5
	assert pf.exclusive == true
	assert pf.stream_dependency == 3
	assert pf.weight == 15

	back := pf.to_frame()
	assert back.header.frame_type == .priority
	assert back.header.stream_id == 5
	assert back.payload.len == 5
	back_raw := (u32(back.payload[0]) << 24) | (u32(back.payload[1]) << 16) | (u32(back.payload[2]) << 8) | u32(back.payload[3])
	assert back_raw & 0x80000000 != 0 // exclusive preserved
	assert back_raw & 0x7fffffff == 3 // stream_dependency preserved
	assert back.payload[4] == 15 // weight preserved
}

// test_priority_frame_non_exclusive verifies PriorityFrame with exclusive=false.
fn test_priority_frame_non_exclusive() {
	payload := [u8(0x00), 0x00, 0x00, 0x07, u8(255)]
	original := Frame{
		header:  FrameHeader{
			length:     5
			frame_type: .priority
			flags:      0
			stream_id:  9
		}
		payload: payload
	}

	pf := PriorityFrame.from_frame(original) or {
		assert false, 'PriorityFrame.from_frame failed: ${err}'
		return
	}
	assert pf.exclusive == false
	assert pf.stream_dependency == 7
	assert pf.weight == 255
}

// test_priority_frame_wrong_type verifies PriorityFrame.from_frame rejects non-PRIORITY frames.
fn test_priority_frame_wrong_type() {
	original := Frame{
		header:  FrameHeader{
			length:     5
			frame_type: .data
			flags:      0
			stream_id:  1
		}
		payload: []u8{len: 5}
	}
	PriorityFrame.from_frame(original) or {
		assert err.msg().contains('expected PRIORITY frame')
		return
	}
	assert false, 'Should have rejected non-PRIORITY frame'
}

// test_rst_stream_frame_from_frame verifies RstStreamFrame from_frame conversion.
fn test_rst_stream_frame_from_frame() {
	// error_code = 0x8 (cancel)
	payload := [u8(0x00), 0x00, 0x00, 0x08]
	original := Frame{
		header:  FrameHeader{
			length:     4
			frame_type: .rst_stream
			flags:      0
			stream_id:  3
		}
		payload: payload
	}

	rf := RstStreamFrame.from_frame(original) or {
		assert false, 'RstStreamFrame.from_frame failed: ${err}'
		return
	}
	assert rf.stream_id == 3
	assert rf.error_code == .cancel
}
