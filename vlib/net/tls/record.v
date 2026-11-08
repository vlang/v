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

// Encode a TLSPlaintext record header + fragment. Returns header(5)+fragment.
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

// Decode a TLSPlaintext record from a byte slice. Returns the record and the
// number of bytes consumed (header + fragment). Errors on truncated/oversize.
pub fn decode_plain_record(data []u8) !(PlainRecord, int) {
	if data.len < 5 {
		return error('tls: truncated record header')
	}
	ct := unsafe { ContentType(data[0]) }
	ver := (u16(data[1]) << 8) | u16(data[2])
	ln := (u16(data[3]) << 8) | u16(data[4])
	if int(ln) > 16384 + 256 { // RFC 8446 §5.1 plaintext limit (2^14 + 256)
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

// Encode an inner plaintext into the form that gets AEAD-encrypted:
// content || content_type (1 byte) || 0x00*padding.
pub fn encode_inner(pt InnerPlaintext) []u8 {
	mut out := []u8{cap: pt.content.len + 1 + pt.padding_len}
	out << pt.content
	out << u8(pt.content_type)
	for _ in 0 .. pt.padding_len {
		out << u8(0)
	}
	return out
}

// Decode an inner plaintext from decrypted AEAD output. RFC 8446 §5.2:
// strip trailing zeros, last non-zero byte is the content type.
pub fn decode_inner(data []u8) !InnerPlaintext {
	if data.len == 0 {
		return error('tls: empty inner plaintext')
	}
	// find last non-zero
	mut i := data.len - 1
	for i > 0 && data[i] == 0 {
		i--
	}
	// data[i] is content type
	ct := unsafe { ContentType(data[i]) }
	content := data[..i].clone()
	return InnerPlaintext{
		content:      content
		content_type: ct
		padding_len:  (data.len - 1) - i
	}
}

// Encode a ciphertext record (already-encrypted fragment including tag).
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

// Decode a ciphertext record header.
pub fn decode_cipher_record(data []u8) !(CipherRecord, int) {
	if data.len < 5 {
		return error('tls: truncated cipher record header')
	}
	ct := unsafe { ContentType(data[0]) }
	ver := (u16(data[1]) << 8) | u16(data[2])
	ln := (u16(data[3]) << 8) | u16(data[4])
	if 5 + int(ln) > data.len {
		return error('tls: truncated cipher record body')
	}
	frag := data[5..5 + int(ln)].clone()
	return CipherRecord{
		content_type:   ct
		legacy_version: ver
		fragment:       frag
	}, 5 + int(ln)
}

// TLS 1.3 record size limits (RFC 8446 §5.1).
pub const max_plaintext_len = 16384 // 2^14
pub const max_ciphertext_len = max_plaintext_len + 256 // + padding/overhead

// alert record helper: build a 2-byte alert payload.
pub fn alert_bytes(level AlertLevel, desc AlertDescription) []u8 {
	return [u8(level), u8(desc)]
}

// parse_alert decodes a 2-byte alert payload.
pub fn parse_alert(data []u8) !(AlertLevel, AlertDescription) {
	if data.len < 2 {
		return error('tls: truncated alert')
	}
	return unsafe { AlertLevel(data[0]) }, unsafe { AlertDescription(data[1]) }
}
