module quic

// H3ErrorCode is the RFC 9114 §8.1 "HTTP/3 Error Codes" registry, used as
// the QUIC application-protocol error code when abruptly terminating an
// HTTP/3 stream, aborting a stream read, or closing an HTTP/3 connection.
// Exact wire values transcribed directly from §8.1 / Table 4 (§11.2.3),
// not recalled -- every value here was read from the fetched RFC text at
// `.claude/skills/code-review/rfc-texts/rfc9114.txt`.
pub enum H3ErrorCode {
	no_error               = 0x0100
	general_protocol_error = 0x0101
	internal_error         = 0x0102
	stream_creation_error  = 0x0103
	closed_critical_stream = 0x0104
	frame_unexpected       = 0x0105
	frame_error            = 0x0106
	excessive_load         = 0x0107
	id_error               = 0x0108
	settings_error         = 0x0109
	missing_settings       = 0x010a
	request_rejected       = 0x010b
	request_cancelled      = 0x010c
	request_incomplete     = 0x010d
	message_error          = 0x010e
	connect_error          = 0x010f
	version_fallback       = 0x0110
}

// code returns the wire value of e as a u64, ready to carry as a QUIC
// application-protocol error code (RFC 9000 §20.2) in a CONNECTION_CLOSE
// or RESET_STREAM/STOP_SENDING frame.
pub fn (e H3ErrorCode) code() u64 {
	return u64(e)
}
