module v2

// Tests for HPACK and frame serialization basics.

fn test_hpack_encoding_decoding() {
	mut encoder := new_encoder()
	mut decoder := new_decoder()

	headers := [
		HeaderField{
			name:  ':method'
			value: 'GET'
		},
		HeaderField{
			name:  ':scheme'
			value: 'https'
		},
		HeaderField{
			name:  ':path'
			value: '/'
		},
		HeaderField{
			name:  'user-agent'
			value: 'v-http2-client'
		},
		HeaderField{
			name:  'custom-header'
			value: 'custom-value'
		},
	]

	println('Testing HPACK encoding...')
	encoded := encoder.encode(headers)
	assert encoded.len > 0

	println('Testing HPACK decoding...')
	decoded := decoder.decode(encoded) or {
		assert false, 'Decoded failed: ${err}'
		return
	}

	assert decoded.len == headers.len
	for i in 0 .. headers.len {
		assert decoded[i].name == headers[i].name
		assert decoded[i].value == headers[i].value
	}
	println('HPACK test passed')
}

fn test_frame_serialization() {
	println('Testing DATA frame...')
	data_payload := 'Hello HTTP/2'.bytes()
	mut data_frame := DataFrame{
		data: data_payload
	}
	assert data_frame.data.len == 12

	println('Testing HEADERS frame...')
	mut headers_frame := HeadersFrame{
		headers:     [u8(0x82), 0x86, 0x84, 0x41, 0x8c, 0xf1, 0xe3, 0xc2, 0xe5, 0xf2, 0x3a, 0x6b,
			0xa0, 0xab, 0x90, 0xf4, 0xff]
		end_stream:  true
		end_headers: true
	}
	assert headers_frame.end_stream == true
	assert headers_frame.end_headers == true

	println('Testing SETTINGS frame...')
	mut settings_frame := SettingsFrame{
		ack:      false
		settings: {
			u16(SettingId.max_concurrent_streams): u32(100)
			u16(SettingId.initial_window_size):    u32(65535)
		}
	}
	assert settings_frame.ack == false
	assert settings_frame.settings[u16(SettingId.max_concurrent_streams)] == 100

	println('Testing PING frame...')
	mut ping_frame := PingFrame{
		ack:  false
		data: [u8(1), 2, 3, 4, 5, 6, 7, 8]!
	}
	assert ping_frame.data[0] == 1
	assert ping_frame.data[7] == 8

	println('Testing GOAWAY frame...')
	mut goaway_frame := GoAwayFrame{
		last_stream_id: 10
		error_code:     ErrorCode.no_error
		debug_data:     'debug'.bytes()
	}
	assert goaway_frame.last_stream_id == 10
	assert goaway_frame.error_code == .no_error

	println('Frame test passed')
}

fn test_client_max_concurrent_streams_check() {
	mut conn := Connection{}
	conn.remote_settings.max_concurrent_streams = 2

	// No streams yet
	assert conn.active_stream_count() == 0

	// Add one stream — below limit
	conn.streams[u32(1)] = &Stream{
		id:    1
		state: .open
	}
	assert conn.active_stream_count() == 1
	assert conn.active_stream_count() < conn.remote_settings.max_concurrent_streams

	// Add second stream — at limit
	conn.streams[u32(3)] = &Stream{
		id:    3
		state: .open
	}
	assert conn.active_stream_count() == 2
	assert conn.active_stream_count() >= conn.remote_settings.max_concurrent_streams

	// Remove a stream — below limit again
	conn.streams.delete(u32(1))
	assert conn.active_stream_count() == 1
	assert conn.active_stream_count() < conn.remote_settings.max_concurrent_streams

	// Verify unlimited when max is 0
	conn.remote_settings.max_concurrent_streams = 0
	conn.streams[u32(5)] = &Stream{
		id:    5
		state: .open
	}
	conn.streams[u32(7)] = &Stream{
		id:    7
		state: .open
	}
	// With max=0, the check `count >= max && max > 0` should never trigger
	is_blocked := conn.active_stream_count() >= conn.remote_settings.max_concurrent_streams
		&& conn.remote_settings.max_concurrent_streams > 0
	assert !is_blocked, 'max_concurrent_streams=0 should mean unlimited'
}

fn test_client_max_concurrent_streams_error() {
	mut conn := Connection{}
	conn.remote_settings.max_concurrent_streams = 1
	conn.streams[u32(1)] = &Stream{
		id:    1
		state: .open
	}

	// Verify enforce function returns error at limit
	enforce_max_concurrent_streams(&conn) or {
		assert err.msg() == 'max concurrent streams exceeded'
		return
	}
	assert false, 'expected error when at max concurrent streams limit'
}

// --- Task P2-2: INITIAL_WINDOW_SIZE delta adjustment (RFC 7540 §6.9.2) ---

fn test_initial_window_size_change_adjusts_existing_streams() {
	// When INITIAL_WINDOW_SIZE changes via SETTINGS, all existing stream
	// windows must be adjusted by delta (new - old) per RFC 7540 §6.9.2.
	mut conn := Connection{}
	conn.remote_settings.initial_window_size = 65535

	conn.streams[u32(1)] = &Stream{
		id:          1
		state:       .open
		window_size: 65535
	}
	conn.streams[u32(3)] = &Stream{
		id:          3
		state:       .open
		window_size: 50000
	}

	// Server sends new INITIAL_WINDOW_SIZE = 131070 (delta = +65535)
	pairs := [SettingPair{
		id:    .initial_window_size
		value: 131070
	}]
	conn.apply_remote_settings(pairs) or {
		assert false, 'apply_remote_settings should not error: ${err}'
		return
	}

	s1 := conn.streams[u32(1)] or {
		assert false, 'stream 1 not found'
		return
	}
	assert s1.window_size == i64(65535 + 65535), 'stream 1 window should be adjusted by delta +65535, got ${s1.window_size}'

	s3 := conn.streams[u32(3)] or {
		assert false, 'stream 3 not found'
		return
	}
	assert s3.window_size == i64(50000 + 65535), 'stream 3 window should be adjusted by delta +65535, got ${s3.window_size}'
}

fn test_initial_window_size_decrease_adjusts_streams() {
	mut conn := Connection{}
	conn.remote_settings.initial_window_size = 65535

	conn.streams[u32(1)] = &Stream{
		id:          1
		state:       .open
		window_size: 65535
	}

	// Server decreases INITIAL_WINDOW_SIZE to 32767 (delta = -32768)
	pairs := [SettingPair{
		id:    .initial_window_size
		value: 32767
	}]
	conn.apply_remote_settings(pairs) or {
		assert false, 'apply_remote_settings should not error: ${err}'
		return
	}

	s1 := conn.streams[u32(1)] or {
		assert false, 'stream 1 not found'
		return
	}
	assert s1.window_size == i64(65535 - 32768), 'stream 1 window should decrease by delta, got ${s1.window_size}'
}

fn test_initial_window_size_overflow_returns_error() {
	// If adjusting a stream window would exceed 2^31-1, the connection
	// must return FLOW_CONTROL_ERROR per RFC 7540 §6.9.2.
	mut conn := Connection{}
	conn.remote_settings.initial_window_size = 65535

	conn.streams[u32(1)] = &Stream{
		id:          1
		state:       .open
		window_size: 0x7fffffff - 10
	}

	// Delta = 0x7fffffff - 65535, which added to stream 1's window overflows
	pairs := [SettingPair{
		id:    .initial_window_size
		value: 0x7fffffff
	}]
	conn.apply_remote_settings(pairs) or {
		assert err.msg().contains('FLOW_CONTROL_ERROR'), 'expected FLOW_CONTROL_ERROR, got: ${err}'
		return
	}
	assert false, 'should return FLOW_CONTROL_ERROR when stream window overflows'
}
