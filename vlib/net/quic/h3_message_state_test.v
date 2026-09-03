module quic

fn test_is_h3_frame_valid_on_stream_table1_data_and_headers() {
	data := H3Frame(DataFrame{})
	headers := H3Frame(HeadersFrame{})
	assert !is_h3_frame_valid_on_stream(data, .control)
	assert is_h3_frame_valid_on_stream(data, .request)
	assert is_h3_frame_valid_on_stream(data, .push)
	assert !is_h3_frame_valid_on_stream(headers, .control)
	assert is_h3_frame_valid_on_stream(headers, .request)
	assert is_h3_frame_valid_on_stream(headers, .push)
}

fn test_is_h3_frame_valid_on_stream_table1_control_only_frames() {
	control_only := [
		H3Frame(CancelPushFrame{}),
		H3Frame(SettingsFrame{}),
		H3Frame(GoawayFrame{}),
		H3Frame(MaxPushIdFrame{}),
	]
	for f in control_only {
		assert is_h3_frame_valid_on_stream(f, .control)
		assert !is_h3_frame_valid_on_stream(f, .request)
		assert !is_h3_frame_valid_on_stream(f, .push)
	}
}

fn test_is_h3_frame_valid_on_stream_table1_push_promise() {
	pp := H3Frame(PushPromiseFrame{})
	assert !is_h3_frame_valid_on_stream(pp, .control)
	assert is_h3_frame_valid_on_stream(pp, .request)
	assert !is_h3_frame_valid_on_stream(pp, .push)
}

fn test_is_h3_frame_valid_on_stream_reserved_row_is_universal() {
	raw := H3Frame(H3RawFrame{
		frame_type: 0x21
	})
	assert is_h3_frame_valid_on_stream(raw, .control)
	assert is_h3_frame_valid_on_stream(raw, .request)
	assert is_h3_frame_valid_on_stream(raw, .push)
}

fn test_h3_control_stream_state_accepts_settings_first() {
	mut s := new_h3_control_stream_state()
	s.note_frame(H3Frame(SettingsFrame{}))!
	// A subsequent non-SETTINGS frame is fine.
	s.note_frame(H3Frame(GoawayFrame{}))!
}

fn test_h3_control_stream_state_rejects_non_settings_first_frame() {
	mut s := new_h3_control_stream_state()
	if _ := s.note_frame(H3Frame(GoawayFrame{})) {
		assert false, 'expected an error when the first frame is not SETTINGS'
	} else {
		assert err.code() == int(H3ErrorCode.missing_settings)
	}
}

fn test_h3_control_stream_state_rejects_grease_as_first_frame() {
	mut s := new_h3_control_stream_state()
	raw := H3Frame(H3RawFrame{
		frame_type: 0x21
	})
	if _ := s.note_frame(raw) {
		assert false, 'expected an error when the first frame is grease, not SETTINGS'
	} else {
		assert err.code() == int(H3ErrorCode.missing_settings)
	}
}

fn test_h3_control_stream_state_rejects_second_settings_frame() {
	mut s := new_h3_control_stream_state()
	s.note_frame(H3Frame(SettingsFrame{}))!
	if _ := s.note_frame(H3Frame(SettingsFrame{})) {
		assert false, 'expected an error for a second SETTINGS frame'
	} else {
		assert err.code() == int(H3ErrorCode.frame_unexpected)
	}
}

fn test_h3_control_stream_state_rejects_second_settings_frame_after_other_frames() {
	mut s := new_h3_control_stream_state()
	s.note_frame(H3Frame(SettingsFrame{}))!
	s.note_frame(H3Frame(GoawayFrame{}))!
	s.note_frame(H3Frame(CancelPushFrame{}))!
	if _ := s.note_frame(H3Frame(SettingsFrame{})) {
		assert false, 'expected an error for a second SETTINGS frame after other traffic'
	} else {
		assert err.code() == int(H3ErrorCode.frame_unexpected)
	}
}
