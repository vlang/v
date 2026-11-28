// Copyright (c) 2026 blackshirt.
//
// Alert vocabulary tests: the RFC 8446 §6.2 2-byte alert payload round-trips
// through encode/parse_alert, malformed framing is rejected, an unenumerated
// description is preserved verbatim, and QUIC maps it to a CRYPTO_ERROR code
// (RFC 9001 §4.8).
module handshake

// test_alert_close_notify_roundtrip checks the 2-byte alert payload round-trips
// through encode/parse_alert, pinned to literal bytes (RFC 8446 §6.1, §6.2).
fn test_alert_close_notify_roundtrip() ! {
	a := new_alert(.close_notify)
	enc := a.encode()
	assert enc == [u8(0x01), 0x00]

	parsed := parse_alert(enc)!
	assert parsed.level == .warning
	assert parsed.description == .close_notify
}

// test_new_alert_levels checks that close_notify is the only warning-level
// alert; everything else is fatal (RFC 8446 §6.1).
fn test_new_alert_levels() {
	assert new_alert(.close_notify).level == .warning
	assert new_alert(.handshake_failure).level == .fatal
}

// test_parse_alert_rejects_bad_length checks that payloads other than 2 bytes
// are framing errors.
fn test_parse_alert_rejects_bad_length() ! {
	if _ := parse_alert([]u8{}) {
		assert false
	}
	if _ := parse_alert([u8(0x02)]) {
		assert false
	}
	if _ := parse_alert([u8(0x02), 0x00, 0x00]) {
		assert false
	}
}

// test_parse_alert_rejects_invalid_level checks that only warning(1) and
// fatal(2) are valid levels (RFC 8446 §6).
fn test_parse_alert_rejects_invalid_level() ! {
	parse_alert([u8(0x00), 0x00]) or {
		assert err.msg().contains('invalid alert level')
		return
	}
	assert false
}

// test_parse_alert_preserves_unknown_description checks that an unenumerated
// description survives the cast, so re-encoding yields the original bytes.
fn test_parse_alert_preserves_unknown_description() ! {
	a := parse_alert([u8(0x02), 0xfe])!
	assert a.level == .fatal
	assert a.encode() == [u8(0x02), 0xfe]
}

// test_quic_connection_error_mapping checks that an alert surfaces over QUIC as
// 0x100 + the description byte (RFC 9001 §4.8).
fn test_quic_connection_error_mapping() ! {
	assert AlertDescription.close_notify.quic_connection_error() == u32(0x100)
	assert AlertDescription.handshake_failure.quic_connection_error() == u32(0x128)
	assert AlertDescription.decode_error.quic_connection_error() == 0x100 + u32(AlertDescription.decode_error)
}
