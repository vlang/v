module tls

// Record layer (RFC 8446 §5).
//
// TLSPlaintext / TLSInnerPlaintext / TLSCiphertext. This module implements
// the codec only; encryption/decryption of the AEAD payload is handled by the
// handshake/key-schedule module (Phase 2).

// A decoded TLS record header (5 bytes on the wire).
pub struct RecordHeader {
pub:
	content_type   ContentType
	legacy_version u16
	length         u16
}

// A TLSPlaintext record (unencrypted form, used only for the initial
// ClientHello/ServerHello flight before keys are derived, and for alerts).
pub struct PlainRecord {
pub:
	content_type ContentType
	version      u16 = tls_version_1_3
	fragment     []u8
}

// An inner plaintext record (RFC 8446 §5.2): the real content type is carried
// inside the encrypted payload, followed by zero or more zero bytes and a
// final content-type byte.
pub struct InnerPlaintext {
pub:
	content      []u8
	content_type ContentType
	padding_len  int
}

// A ciphertext record (RFC 8446 §5.1): opaque fragment + 16-byte AEAD tag.
pub struct CipherRecord {
pub:
	content_type   ContentType = .application_data // always 23 on the wire
	legacy_version u16         = tls_version_1_3
	// encrypted payload (ciphertext + auth tag) as it appears on the wire.
	fragment []u8
}

// encode_plain_record encodes a TLSPlaintext record header + fragment.
// encode_plain_record returns header(5)+fragment.
pub fn encode_plain_record(r PlainRecord) []u8 {
	mut out := []u8{cap: 5 + r.fragment.len}
	out << u8(r.content_type)
	out << u8((r.version >> 8) & 0xff)
	out << u8(r.version & 0xff)
	out << u8((r.fragment.len >> 8) & 0xff)
	out << u8(r.fragment.len & 0xff)
	out << r.fragment
	return out
}

// decode_plain_record decodes a TLSPlaintext record from a byte slice.
// decode_plain_record returns the record and the bytes consumed (header + fragment). Errors on truncated/oversize.
pub fn decode_plain_record(data []u8) !(PlainRecord, int) {
	if data.len < 5 {
		return error('tls: truncated record header')
	}
	if data[0] != u8(ContentType.change_cipher_spec) && data[0] != u8(ContentType.alert)
		&& data[0] != u8(ContentType.handshake) && data[0] != u8(ContentType.application_data) {
		return error('tls: invalid outer content type')
	}
	ct := unsafe { ContentType(data[0]) }
	ver := (u16(data[1]) << 8) | u16(data[2])
	// RFC 8446 §5.1: peers send the initial ClientHello record with 0x0301;
	// accept 0x0301..0x0303 on plaintext records.
	if ver != tls_version_1_3 && ver != 0x0301 && ver != 0x0302 {
		return error('tls: invalid legacy version')
	}
	ln := (u16(data[3]) << 8) | u16(data[4])
	if int(ln) > max_plaintext_len { // RFC 8446 §5.1 plaintext limit (2^14)
		return error('tls: record too large')
	}
	if 5 + int(ln) > data.len {
		return error('tls: truncated record body')
	}
	frag := data[5..5 + int(ln)].clone()
	return PlainRecord{
		content_type: ct
		version:      ver
		fragment:     frag
	}, 5 + int(ln)
}

// encode_inner encodes an inner plaintext into the form that gets AEAD-encrypted:
// encode_inner returns content || content_type (1 byte) || 0x00*padding.
pub fn encode_inner(pt InnerPlaintext) []u8 {
	mut out := []u8{cap: pt.content.len + 1 + pt.padding_len}
	out << pt.content
	out << u8(pt.content_type)
	for _ in 0 .. pt.padding_len {
		out << u8(0)
	}
	return out
}

// decode_inner decodes an inner plaintext from decrypted AEAD output. RFC 8446 §5.2:
// decode_inner strips trailing zeros; the last non-zero byte is the content type.
// decode_inner rejects empty input, all-padding input and unknown content types.
pub fn decode_inner(data []u8) !InnerPlaintext {
	if data.len == 0 {
		return error('tls: empty inner plaintext')
	}
	// find last non-zero
	mut i := data.len - 1
	for i > 0 && data[i] == 0 {
		i--
	}
	if data[i] == 0 {
		return error('tls: invalid inner content type')
	}
	// data[i] is content type; only the four RFC 8446 types are valid.
	if data[i] != u8(ContentType.change_cipher_spec) && data[i] != u8(ContentType.alert)
		&& data[i] != u8(ContentType.handshake) && data[i] != u8(ContentType.application_data) {
		return error('tls: invalid inner content type')
	}
	ct := unsafe { ContentType(data[i]) }
	content := data[..i].clone()
	return InnerPlaintext{
		content:      content
		content_type: ct
		padding_len:  (data.len - 1) - i
	}
}

// encode_cipher_record encodes a ciphertext record (already-encrypted fragment including tag).
pub fn encode_cipher_record(r CipherRecord) []u8 {
	mut out := []u8{cap: 5 + r.fragment.len}
	out << u8(r.content_type)
	out << u8((r.legacy_version >> 8) & 0xff)
	out << u8(r.legacy_version & 0xff)
	out << u8((r.fragment.len >> 8) & 0xff)
	out << u8(r.fragment.len & 0xff)
	out << r.fragment
	return out
}

// decode_cipher_record decodes a ciphertext record header.
// decode_cipher_record validates the outer type and the RFC 8446 §5.1 size cap.
pub fn decode_cipher_record(data []u8) !(CipherRecord, int) {
	if data.len < 5 {
		return error('tls: truncated cipher record header')
	}
	if data[0] != u8(ContentType.application_data) {
		return error('tls: invalid outer content type')
	}
	ver := (u16(data[1]) << 8) | u16(data[2])
	if ver != tls_version_1_3 {
		return error('tls: invalid legacy version')
	}
	ln := (u16(data[3]) << 8) | u16(data[4])
	if int(ln) > max_ciphertext_len {
		return error('tls: record too large')
	}
	if 5 + int(ln) > data.len {
		return error('tls: truncated cipher record body')
	}
	frag := data[5..5 + int(ln)].clone()
	return CipherRecord{
		content_type:   .application_data
		legacy_version: ver
		fragment:       frag
	}, 5 + int(ln)
}

// TLS 1.3 record size limits (RFC 8446 §5.1).
pub const max_plaintext_len = 16384 // 2^14
pub const max_ciphertext_len = max_plaintext_len + 256 // + padding/overhead

// alert_bytes builds a 2-byte alert payload.
pub fn alert_bytes(level AlertLevel, desc AlertDescription) []u8 {
	return [u8(level), u8(desc)]
}

// parse_alert decodes a 2-byte alert payload into level and description.
// parse_alert validates that both bytes name known alert values.
pub fn parse_alert(data []u8) !(AlertLevel, AlertDescription) {
	if data.len < 2 {
		return error('tls: truncated alert')
	}
	if data[0] != u8(AlertLevel.warning) && data[0] != u8(AlertLevel.fatal) {
		return error('tls: invalid alert level')
	}
	match data[1] {
		u8(AlertDescription.close_notify), u8(AlertDescription.unexpected_message),
		u8(AlertDescription.bad_record_mac), u8(AlertDescription.record_overflow),
		u8(AlertDescription.handshake_failure), u8(AlertDescription.bad_certificate),
		u8(AlertDescription.unsupported_certificate), u8(AlertDescription.certificate_revoked),
		u8(AlertDescription.certificate_expired), u8(AlertDescription.certificate_unknown),
		u8(AlertDescription.illegal_parameter), u8(AlertDescription.unknown_ca),
		u8(AlertDescription.access_denied), u8(AlertDescription.decode_error),
		u8(AlertDescription.decrypt_error), u8(AlertDescription.protocol_version),
		u8(AlertDescription.insufficient_security), u8(AlertDescription.internal_error),
		u8(AlertDescription.inappropriate_fallback), u8(AlertDescription.user_canceled),
		u8(AlertDescription.missing_extension), u8(AlertDescription.unsupported_extension),
		u8(AlertDescription.unrecognized_name),
		u8(AlertDescription.bad_certificate_status_response),
		u8(AlertDescription.unknown_psk_identity), u8(AlertDescription.certificate_required),
		u8(AlertDescription.no_application_protocol) {
		}
		else {
			return error('tls: invalid alert description')
		}
	}
	return unsafe { AlertLevel(data[0]) }, unsafe { AlertDescription(data[1]) }
}
