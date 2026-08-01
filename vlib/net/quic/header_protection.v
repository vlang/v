module quic

import crypto.aes

// RFC 9001 §5.4 — QUIC header protection. Applies to Initial, Handshake,
// 0-RTT, and 1-RTT packets (never to Retry or Version Negotiation, which have
// no packet-number field to protect at all). v1 pins TLS_AES_128_GCM_SHA256
// (see tls13_client_hello.v), so header protection here always uses the
// AES-based construction (RFC 9001 §5.4.3): mask = ECB-Encrypt(hp_key,
// sample), a single AES block encryption of a 16-byte sample with no
// chaining.
//
// The sample is always taken assuming the packet number field occupies its
// maximum possible length (4 bytes) — RFC 9001 §5.4.2 requires this
// specifically so a RECEIVER, who does not yet know the real (protected)
// packet-number length, can still locate the sample deterministically. This
// is why sample location and packet-number length are handled as two
// distinct steps below, never conflated.

const header_protection_sample_size = 16

// pn_max_length is the fixed offset (in bytes, RFC 9001 §5.4.2) from the
// start of the (as-yet-unknown-length) packet number field to where the
// header protection sample begins — independent of the packet's ACTUAL
// packet number length, which is itself part of what header protection
// hides.
const pn_max_length = 4

// header_protection_mask computes the RFC 9001 §5.4.3 AES-based header
// protection mask for one 16-byte `sample`. Only the first 5 bytes of the
// returned 16-byte mask are ever used by callers (mask[0] for the protected
// header bits, mask[1..5] for up to 4 packet-number bytes) — the AES-ECB
// construction always produces a full 16-byte block regardless.
fn header_protection_mask(hp_key []u8, sample []u8) ![]u8 {
	if hp_key.len != 16 {
		return error('quic: header protection key must be 16 bytes for AES-128, got ${hp_key.len}')
	}
	if sample.len != header_protection_sample_size {
		return error('quic: header protection sample must be ${header_protection_sample_size} bytes, got ${sample.len}')
	}
	block := aes.new_cipher(hp_key)
	mut mask := []u8{len: header_protection_sample_size}
	block.encrypt(mut mask, sample)
	return mask
}

// protected_bits_mask returns which low bits of the first byte header
// protection covers: 4 bits (reserved + packet-number length, RFC 9000
// §17.2) for a long header, 5 bits (reserved + key phase + packet-number
// length, RFC 9000 §17.3.1) for a short header.
fn protected_bits_mask(form HeaderForm) u8 {
	return if form == .long { u8(0x0f) } else { u8(0x1f) }
}

// reserved_bits_mask returns the Reserved Bits within the protected low
// bits: bits 3-2 (0x0c) for a long header (RFC 9000 §17.2), bits 4-3 (0x18)
// for a short header (RFC 9000 §17.3.1) -- distinct from the packet-number
// length bits and, for a short header, the key phase bit, which share the
// same byte but carry no such "must be zero" requirement.
fn reserved_bits_mask(form HeaderForm) u8 {
	return if form == .long { u8(0x0c) } else { u8(0x18) }
}

// protect_header applies header protection to `packet` in place, given an
// already-chosen `pn_length` — the sender always knows this, since it picked
// the packet number's encoded length itself before calling this (see
// encode_packet_number). `packet` must already contain the full unprotected
// header AND the AEAD-encrypted payload: per RFC 9001 §5.4.1/§5.3, the sample
// is taken from the CIPHERTEXT, so this function must run strictly AFTER
// AEAD encryption — reversing that order is the single most common QUIC
// packet-protection implementation bug. `pn_offset` is the index into
// `packet` where the (currently still plaintext) packet number bytes begin.
pub fn protect_header(mut packet []u8, pn_offset int, pn_length int, hp_key []u8, form HeaderForm) ! {
	if pn_length < 1 || pn_length > pn_max_length {
		return error('quic: invalid packet number length ${pn_length}, must be 1-4')
	}
	if pn_offset < 0 || pn_offset + pn_max_length + header_protection_sample_size > packet.len {
		return error('quic: packet too short to sample for header protection (need ${pn_offset +
			pn_max_length + header_protection_sample_size} bytes, have ${packet.len})')
	}
	sample_offset := pn_offset + pn_max_length
	sample := unsafe { packet[sample_offset..sample_offset + header_protection_sample_size] }
	mask := header_protection_mask(hp_key, sample)!
	packet[0] ^= mask[0] & protected_bits_mask(form)
	for i in 0 .. pn_length {
		packet[pn_offset + i] ^= mask[1 + i]
	}
}

// unprotect_header removes header protection from `packet` in place and
// returns the packet number's true encoded length. Unlike `protect_header`,
// the caller does NOT know `pn_length` up front — it is itself part of the
// protected first byte — so this function must unmask the first byte FIRST
// to learn `pn_length`, then unmask exactly that many packet-number bytes.
// Doing this in the opposite order (unmasking a guessed number of
// packet-number bytes before the first byte has revealed the real count)
// would corrupt bytes belonging to the payload rather than the packet
// number.
//
// Once unmasked, the first byte's Reserved Bits are checked: RFC 9000
// §17.2/§17.3.1 requires a sender to always transmit them as zero, and
// requires a receiver to treat a non-zero value here as a PROTOCOL_VIOLATION
// connection error, NOT a silently dropped packet (unlike an AEAD
// authentication failure, see decrypt_packet_payload's doc comment) --
// mapping this error to that specific close behavior is the caller's
// (Phase 8+ connection-lifecycle) responsibility, same division as this
// module's other errors.
pub fn unprotect_header(mut packet []u8, pn_offset int, hp_key []u8, form HeaderForm) !int {
	if pn_offset < 0 || pn_offset + pn_max_length + header_protection_sample_size > packet.len {
		return error('quic: packet too short to sample for header protection (need ${pn_offset +
			pn_max_length + header_protection_sample_size} bytes, have ${packet.len})')
	}
	sample_offset := pn_offset + pn_max_length
	sample := unsafe { packet[sample_offset..sample_offset + header_protection_sample_size] }
	mask := header_protection_mask(hp_key, sample)!
	packet[0] ^= mask[0] & protected_bits_mask(form)
	if packet[0] & reserved_bits_mask(form) != 0 {
		return error('quic: non-zero reserved bits after header protection removal (RFC 9000 §17.2/§17.3.1 PROTOCOL_VIOLATION)')
	}
	pn_length := int(packet[0] & 0x03) + 1
	for i in 0 .. pn_length {
		packet[pn_offset + i] ^= mask[1 + i]
	}
	return pn_length
}
