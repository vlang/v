module v3

// HTTP/3 error codes as defined in RFC 9114 §8.1.

// H3ErrorCode represents HTTP/3 application error codes sent in
// QUIC CONNECTION_CLOSE or RESET_STREAM frames (RFC 9114 §8.1).
pub enum H3ErrorCode as u64 {
	h3_no_error               = 0x0100
	h3_general_protocol_error = 0x0101
	h3_internal_error         = 0x0102
	h3_stream_creation_error  = 0x0103
	h3_closed_critical_stream = 0x0104
	h3_frame_unexpected       = 0x0105
	h3_frame_error            = 0x0106
	h3_excessive_load         = 0x0107
	h3_id_error               = 0x0108
	h3_settings_error         = 0x0109
	h3_missing_settings       = 0x010a
	h3_request_rejected       = 0x010b
	h3_request_cancelled      = 0x010c
	h3_request_incomplete     = 0x010d
	h3_message_error          = 0x010e
	h3_connect_error          = 0x010f
	h3_version_fallback       = 0x0110
}

// str returns a human-readable name for the error code.
pub fn (e H3ErrorCode) str() string {
	return match e {
		.h3_no_error { 'H3_NO_ERROR' }
		.h3_general_protocol_error { 'H3_GENERAL_PROTOCOL_ERROR' }
		.h3_internal_error { 'H3_INTERNAL_ERROR' }
		.h3_stream_creation_error { 'H3_STREAM_CREATION_ERROR' }
		.h3_closed_critical_stream { 'H3_CLOSED_CRITICAL_STREAM' }
		.h3_frame_unexpected { 'H3_FRAME_UNEXPECTED' }
		.h3_frame_error { 'H3_FRAME_ERROR' }
		.h3_excessive_load { 'H3_EXCESSIVE_LOAD' }
		.h3_id_error { 'H3_ID_ERROR' }
		.h3_settings_error { 'H3_SETTINGS_ERROR' }
		.h3_missing_settings { 'H3_MISSING_SETTINGS' }
		.h3_request_rejected { 'H3_REQUEST_REJECTED' }
		.h3_request_cancelled { 'H3_REQUEST_CANCELLED' }
		.h3_request_incomplete { 'H3_REQUEST_INCOMPLETE' }
		.h3_message_error { 'H3_MESSAGE_ERROR' }
		.h3_connect_error { 'H3_CONNECT_ERROR' }
		.h3_version_fallback { 'H3_VERSION_FALLBACK' }
	}
}

// validate_header_names_lowercase checks that all header field names are lowercase
// as required by RFC 9114 §4.2. Returns a stream error with H3_MESSAGE_ERROR
// if any header name contains uppercase letters.
pub fn validate_header_names_lowercase(headers []HeaderField) ! {
	for h in headers {
		if h.name != h.name.to_lower() {
			return error('H3_MESSAGE_ERROR: header field name contains uppercase: "${h.name}" (RFC 9114 §4.2)')
		}
	}
}
