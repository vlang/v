module quic

// qpack_settings_max_table_capacity_id is SETTINGS_QPACK_MAX_TABLE_CAPACITY
// (RFC 9204 §5, §8.1). It is the equivalent of HTTP/2's
// SETTINGS_HEADER_TABLE_SIZE.
pub const qpack_settings_max_table_capacity_id = u64(0x01)

// qpack_settings_blocked_streams_id is SETTINGS_QPACK_BLOCKED_STREAMS (RFC
// 9204 §5, §8.1).
pub const qpack_settings_blocked_streams_id = u64(0x07)

// qpack_max_table_capacity_from_settings extracts
// SETTINGS_QPACK_MAX_TABLE_CAPACITY from an already-decoded HTTP/3 SETTINGS
// frame's parameter list, defaulting to 0 (RFC 9204 §5) when absent. Pure
// function over `h3_frame.v`'s existing `H3Setting` list -- this file adds
// no new frame parsing, only QPACK's own interpretation of values
// `decode_h3_settings_payload` already validated and stored generically.
pub fn qpack_max_table_capacity_from_settings(settings []H3Setting) u64 {
	for s in settings {
		if s.identifier == qpack_settings_max_table_capacity_id {
			return s.value
		}
	}
	return 0
}

// qpack_blocked_streams_from_settings extracts
// SETTINGS_QPACK_BLOCKED_STREAMS from an already-decoded HTTP/3 SETTINGS
// frame's parameter list, defaulting to 0 (RFC 9204 §5) when absent.
pub fn qpack_blocked_streams_from_settings(settings []H3Setting) u64 {
	for s in settings {
		if s.identifier == qpack_settings_blocked_streams_id {
			return s.value
		}
	}
	return 0
}
