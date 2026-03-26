module v2

// Tests for HTTP/2 frame encoding, decoding, validation, and typed frame conversions.

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

fn test_frame_type_from_byte_unknown() {
	result := frame_type_from_byte(0xff)
	assert result == none
}

fn test_parse_frame_header_unknown_type() {
	mut raw := []u8{len: 9}
	raw[0] = 0
	raw[1] = 0
	raw[2] = 5
	raw[3] = 0xfe
	raw[4] = 0
	raw[5] = 0
	raw[6] = 0
	raw[7] = 0
	raw[8] = 1
	header := parse_frame_header(raw) or { return }
	_ = header
}

fn test_frame_validation() {
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

fn test_parse_settings_payload_valid() {
	mut payload := []u8{}
	payload << [u8(0x00), 0x01, 0x00, 0x00, 0x20, 0x00]
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

fn test_parse_settings_payload_skips_unknown() {
	mut payload := []u8{}
	payload << [u8(0x00), 0xFF, 0x00, 0x00, 0x00, 0x2A]
	payload << [u8(0x00), 0x05, 0x00, 0x00, 0x80, 0x00]

	pairs := parse_settings_payload(payload) or {
		assert false, 'parse_settings_payload failed: ${err}'
		return
	}

	assert pairs.len == 1
	assert pairs[0].id == .max_frame_size
	assert pairs[0].value == 32768
}

fn test_parse_settings_payload_empty() {
	pairs := parse_settings_payload([]) or {
		assert false, 'parse_settings_payload failed on empty: ${err}'
		return
	}
	assert pairs.len == 0
}

fn test_parse_settings_payload_incomplete() {
	payload := [u8(0x00), 0x01, 0x00, 0x00]
	parse_settings_payload(payload) or {
		assert err.msg().contains('incomplete')
		return
	}
	assert false, 'Should have rejected incomplete settings payload'
}

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

fn test_settings_frame_roundtrip() {
	mut payload := []u8{}
	payload << [u8(0x00), 0x01, 0x00, 0x00, 0x20, 0x00]
	payload << [u8(0x00), 0x03, 0x00, 0x00, 0x00, 0xC8]

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

fn test_goaway_frame_from_frame() {
	mut payload := []u8{}
	payload << [u8(0x00), 0x00, 0x00, 0x05]
	payload << [u8(0x00), 0x00, 0x00, 0x00]
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

fn test_window_update_frame_from_frame() {
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
		payload: [u8(0x00), 0x00, 0x80, 0x00]
	}
	conn.apply_window_update(wu_frame) or {
		assert false, 'apply_window_update failed: ${err}'
		return
	}
	assert conn.remote_window_size == 65535 + 32768, 'expected ${65535 + 32768}, got ${conn.remote_window_size}'
}

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
		payload: [u8(0x00), 0x01, 0x00, 0x00]
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

fn test_split_data_for_flow_control() {
	data := []u8{len: 100, init: u8(0x41)}

	chunks := split_data_for_window(data, 200, 16384)
	assert chunks.len == 1, 'expected 1 chunk when window > data, got ${chunks.len}'
	assert chunks[0].len == 100

	chunks2 := split_data_for_window(data, 30, 16384)
	mut total := 0
	for chunk in chunks2 {
		assert chunk.len <= 30, 'chunk exceeds window: ${chunk.len} > 30'
		total += chunk.len
	}
	assert total == 100, 'total bytes mismatch: want 100, got ${total}'

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

fn test_split_data_for_flow_control_zero_window() {
	data := []u8{len: 50, init: u8(0x42)}
	chunks := split_data_for_window(data, 0, 16384)
	assert chunks.len == 0, 'expected 0 chunks when window is 0, got ${chunks.len}'
}

fn test_priority_frame_roundtrip() {
	mut payload := []u8{len: 5}
	payload[0] = 0x80
	payload[1] = 0x00
	payload[2] = 0x00
	payload[3] = 0x03
	payload[4] = 15

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
	assert back_raw & 0x80000000 != 0
	assert back_raw & 0x7fffffff == 3
	assert back.payload[4] == 15
}

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

fn test_new_settings_ack_frame() {
	frame := new_settings_ack_frame()
	assert frame.header.frame_type == .settings
	assert frame.header.flags == u8(FrameFlags.ack)
	assert frame.header.stream_id == 0
	assert frame.header.length == 0
	assert frame.payload.len == 0
}

fn test_new_settings_ack_frame_validates() {
	frame := new_settings_ack_frame()
	frame.validate() or {
		assert false, 'SETTINGS ACK frame should be valid: ${err}'
		return
	}
}

fn test_validate_setting_value_max_frame_size_below_minimum() {
	validate_setting_value(.max_frame_size, 16383) or {
		assert err.msg().contains('PROTOCOL_ERROR')
		return
	}
	assert false, 'max_frame_size below 16384 should be rejected'
}

fn test_validate_setting_value_max_frame_size_above_maximum() {
	validate_setting_value(.max_frame_size, 16777216) or {
		assert err.msg().contains('PROTOCOL_ERROR')
		return
	}
	assert false, 'max_frame_size above 16777215 should be rejected'
}

fn test_validate_setting_value_max_frame_size_at_boundaries() {
	validate_setting_value(.max_frame_size, 16384) or {
		assert false, 'max_frame_size 16384 should be valid: ${err}'
		return
	}
	validate_setting_value(.max_frame_size, 16777215) or {
		assert false, 'max_frame_size 16777215 should be valid: ${err}'
		return
	}
}

fn test_validate_setting_value_initial_window_size_overflow() {
	validate_setting_value(.initial_window_size, 2147483648) or {
		assert err.msg().contains('FLOW_CONTROL_ERROR')
		return
	}
	assert false, 'initial_window_size above 2^31-1 should be rejected'
}

fn test_validate_setting_value_initial_window_size_valid() {
	validate_setting_value(.initial_window_size, 2147483647) or {
		assert false, 'initial_window_size 2^31-1 should be valid: ${err}'
		return
	}
	validate_setting_value(.initial_window_size, 65535) or {
		assert false, 'initial_window_size 65535 should be valid: ${err}'
		return
	}
}

fn test_rst_stream_frame_from_frame() {
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
