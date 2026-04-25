module v2

// HTTP/2 SETTINGS frame utilities: validation, conversion, and ACK construction (RFC 7540 §6.5).

// new_settings_ack_frame creates a SETTINGS ACK frame per RFC 7540 §6.5.
pub fn new_settings_ack_frame() Frame {
	return Frame{
		header:  FrameHeader{
			length:     0
			frame_type: .settings
			flags:      u8(FrameFlags.ack)
			stream_id:  0
		}
		payload: []u8{}
	}
}

// validate_setting_value validates a single setting value per RFC 7540 §6.5.2.
pub fn validate_setting_value(id SettingId, value u32) ! {
	match id {
		.enable_push {
			if value > 1 {
				return error('PROTOCOL_ERROR: ENABLE_PUSH must be 0 or 1')
			}
		}
		.max_frame_size {
			if value < default_frame_size || value > max_frame_size {
				return error('PROTOCOL_ERROR: max_frame_size ${value} outside valid range ${default_frame_size}..${max_frame_size}')
			}
		}
		.initial_window_size {
			if value > 0x7fffffff {
				return error('FLOW_CONTROL_ERROR: initial_window_size ${value} exceeds maximum 2^31-1')
			}
		}
		else {}
	}
}

// setting_id_from_u16 converts a u16 to a SettingId enum value.
// Returns none for unknown settings per RFC 7540 §6.5.2 (unknown settings MUST be ignored).
pub fn setting_id_from_u16(id u16) ?SettingId {
	return match id {
		0x1 { .header_table_size }
		0x2 { .enable_push }
		0x3 { .max_concurrent_streams }
		0x4 { .initial_window_size }
		0x5 { .max_frame_size }
		0x6 { .max_header_list_size }
		else { none }
	}
}
