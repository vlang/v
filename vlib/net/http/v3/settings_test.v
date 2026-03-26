module v3

// Tests for RFC 9114 §7.2.4.1: HTTP/2 settings identifiers forbidden in HTTP/3.

// new_settings_test_conn creates a minimal ServerConnection for settings tests.
fn new_settings_test_conn() ServerConnection {
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

// encode_setting_pair encodes a single setting ID-value pair as varints.
fn encode_setting_pair(setting_id u64, setting_value u64) ![]u8 {
	mut payload := []u8{}
	payload << encode_varint(setting_id)!
	payload << encode_varint(setting_value)!
	return payload
}

// ── Forbidden HTTP/2 setting IDs (RFC 9114 §7.2.4.1) ──

fn test_settings_rejects_h2_enable_push() {
	mut s := Server{}
	mut conn := new_settings_test_conn()
	payload := encode_setting_pair(0x02, 1) or {
		assert false, 'failed to build payload: ${err}'
		return
	}
	s.handle_settings_frame(mut conn, payload) or {
		assert err.msg().contains('H3_SETTINGS_ERROR')
		return
	}
	assert false, 'setting ID 0x02 (ENABLE_PUSH) must be rejected'
}

fn test_settings_rejects_h2_max_concurrent_streams() {
	mut s := Server{}
	mut conn := new_settings_test_conn()
	payload := encode_setting_pair(0x03, 100) or {
		assert false, 'failed to build payload: ${err}'
		return
	}
	s.handle_settings_frame(mut conn, payload) or {
		assert err.msg().contains('H3_SETTINGS_ERROR')
		return
	}
	assert false, 'setting ID 0x03 (MAX_CONCURRENT_STREAMS) must be rejected'
}

fn test_settings_rejects_h2_initial_window_size() {
	mut s := Server{}
	mut conn := new_settings_test_conn()
	payload := encode_setting_pair(0x04, 65535) or {
		assert false, 'failed to build payload: ${err}'
		return
	}
	s.handle_settings_frame(mut conn, payload) or {
		assert err.msg().contains('H3_SETTINGS_ERROR')
		return
	}
	assert false, 'setting ID 0x04 (INITIAL_WINDOW_SIZE) must be rejected'
}

fn test_settings_rejects_h2_max_frame_size() {
	mut s := Server{}
	mut conn := new_settings_test_conn()
	payload := encode_setting_pair(0x05, 16384) or {
		assert false, 'failed to build payload: ${err}'
		return
	}
	s.handle_settings_frame(mut conn, payload) or {
		assert err.msg().contains('H3_SETTINGS_ERROR')
		return
	}
	assert false, 'setting ID 0x05 (MAX_FRAME_SIZE) must be rejected'
}

// ── Valid HTTP/3 setting IDs (must still be accepted) ──

fn test_settings_accepts_qpack_max_table_capacity() {
	mut s := Server{}
	mut conn := new_settings_test_conn()
	payload := encode_setting_pair(0x01, 4096) or {
		assert false, 'failed to build payload: ${err}'
		return
	}
	s.handle_settings_frame(mut conn, payload) or {
		assert false, 'setting ID 0x01 should be accepted: ${err}'
		return
	}
}

fn test_settings_accepts_max_field_section_size() {
	mut s := Server{}
	mut conn := new_settings_test_conn()
	payload := encode_setting_pair(0x06, 8192) or {
		assert false, 'failed to build payload: ${err}'
		return
	}
	s.handle_settings_frame(mut conn, payload) or {
		assert false, 'setting ID 0x06 should be accepted: ${err}'
		return
	}
}

fn test_settings_accepts_qpack_blocked_streams() {
	mut s := Server{}
	mut conn := new_settings_test_conn()
	payload := encode_setting_pair(0x07, 100) or {
		assert false, 'failed to build payload: ${err}'
		return
	}
	s.handle_settings_frame(mut conn, payload) or {
		assert false, 'setting ID 0x07 should be accepted: ${err}'
		return
	}
}
