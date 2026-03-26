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
