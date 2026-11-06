// Copyright (c) 2026 blackshirt.
//
// record.v frames and (re)assembles the TLS 1.3 record layer (RFC 8446 §5);
// per-direction AEAD sequence state rides in record_protection.v.
module tls

import x.net.tls.wire

// ContentType identifies the type of message in a TLS record (RFC 8446 §5.1).
enum ContentType as u8 {
	invalid            = 0 // sentinel for an unset type; never valid on the wire
	change_cipher_spec = 20 // TLS 1.3 keeps this only as a middlebox-compat dummy
	alert              = 21
	handshake          = 22
	application_data   = 23 // also the outer type of every encrypted record
}

// content_type_from_u8 decodes a 1-byte record content type.
fn content_type_from_u8(b u8) !ContentType {
	return match b {
		0 { .invalid }
		20 { .change_cipher_spec }
		21 { .alert }
		22 { .handshake }
		23 { .application_data }
		else { error('record: invalid content type ${b}') }
	}
}

// Legacy record version mandated by RFC 8446 §5.1 for middlebox compatibility;
// TLS 1.3 always writes 0x0303 (TLS 1.2) in the record header.
const legacy_record_version = u16(0x0303) // TLS 1.2

// Wire-size limits for a single record (RFC 8446 §5.2): a plaintext record carries
// at most 2^14 bytes; the decrypted content is bounded by 2^14, but a received
// ciphertext may be larger by the inner type byte, the AEAD tag and padding, and
// §5.2 caps the whole record at 2^14 + 1024. Rejecting anything above that would
// drop a conforming peer's padded records, so the receive bound keeps the full slack.
const record_header_len = 5
const max_plaintext_record_len = 16384 // 2^14 bytes
const max_record_overhead = 1024 // inner type byte + AEAD tag + padding (RFC 8446 §5.2 receive bound)
const max_ciphertext_record_len = max_plaintext_record_len + max_record_overhead

// Record is a raw TLS record frame: the 5-byte header plus its opaque payload.
struct Record {
mut:
	content_type   ContentType // kind of message carried in the payload
	legacy_version u16         // always legacy_record_version on the wire; stored on parse but never validated — RFC 8446 §5.1 declares it meaningless, so adding a check here would break compliant peers
	payload        []u8        // record body: handshake bytes, alert bytes, or ciphertext
}

// new_record constructs a valid Record.
fn new_record(content_type ContentType, payload []u8) Record {
	return Record{
		content_type:   content_type
		legacy_version: legacy_record_version
		payload:        payload
	}
}

// change_cipher_spec builds the dummy single-byte ChangeCipherSpec record sent
// after ClientHello for middlebox compatibility (RFC 8446 App. D.4).
fn change_cipher_spec() Record {
	return new_record(.change_cipher_spec, [u8(0x01)])
}

// serialize_into appends the record's wire form — [1 byte ContentType] [2 bytes
// Legacy Version] [2 bytes Length] [Payload] — onto the end of `out`.
fn (r &Record) serialize_into(mut out []u8) {
	out << u8(r.content_type)
	out << u8((r.legacy_version >> 8) & 0xff)
	out << u8(r.legacy_version & 0xff)
	out << u8((r.payload.len >> 8) & 0xff)
	out << u8(r.payload.len & 0xff)
	out << r.payload
}

// serialize encodes the record into its own wire-format byte slice; the length is
// cast without a re-check because every caller bounds the payload by construction.
fn (r &Record) serialize() []u8 {
	mut buf := []u8{cap: record_header_len + r.payload.len}
	r.serialize_into(mut buf)
	return buf
}

// read_record reads a single TLS record: a 5-byte header (content type, legacy
// version, payload length) followed by `length` payload bytes (RFC 8446 §5.2).
fn read_record(mut r wire.Reader) !Record {
	if r.remaining() < record_header_len {
		return error('record: buffer too short for TLS record header (${r.remaining()} < ${record_header_len})')
	}
	raw_type := r.read_u8()!
	content_type := content_type_from_u8(raw_type)!
	// The version is stored but not validated: RFC 8446 §5.1 says it is meaningless.
	version := r.read_u16()!
	length := int(r.read_u16()!)

	if length > max_ciphertext_record_len {
		return error('record: record length ${length} exceeds maximum allowed ${max_ciphertext_record_len}')
	}

	if r.remaining() < length {
		return error('record: incomplete record payload: need ${length} bytes, have ${r.remaining()}')
	}

	payload := r.read_bytes(length)!
	return Record{
		content_type:   content_type
		legacy_version: version
		payload:        payload
	}
}
