module quic

import crypto.rand

fn test_protect_header_then_unprotect_header_round_trips_long_header_all_pn_lengths() {
	hp_key := rand.bytes(16)!
	for pn_length in 1 .. 5 {
		mut packet := []u8{}
		packet << u8(0xc0 | (pn_length - 1)) // long header, Initial type; low bits carry pn_length-1
		packet << [u8(0xde), 0xad, 0xbe, 0xef] // stand-in for the version/dcid/scid/length bytes
		pn_offset := packet.len
		for i in 0 .. pn_length {
			packet << u8(0x10 + i) // arbitrary plaintext packet-number bytes
		}
		packet << []u8{len: 32, init: 0x42} // stand-in AEAD ciphertext (>= 4+16 bytes past pn_offset)
		original := packet.clone()

		protect_header(mut packet, pn_offset, pn_length, hp_key, .long)!
		recovered_pn_length := unprotect_header(mut packet, pn_offset, hp_key, .long)!
		assert recovered_pn_length == pn_length
		assert packet == original
	}
}

fn test_protect_header_then_unprotect_header_round_trips_short_header_all_pn_lengths() {
	hp_key := rand.bytes(16)!
	dcid := rand.bytes(8)!
	for pn_length in 1 .. 5 {
		mut packet := []u8{}
		packet << u8(0x40 | (pn_length - 1)) // short header; fixed bit set, form bit clear
		packet << dcid
		pn_offset := packet.len
		for i in 0 .. pn_length {
			packet << u8(0x20 + i)
		}
		packet << []u8{len: 32, init: 0x77}
		original := packet.clone()

		protect_header(mut packet, pn_offset, pn_length, hp_key, .short)!
		recovered_pn_length := unprotect_header(mut packet, pn_offset, hp_key, .short)!
		assert recovered_pn_length == pn_length
		assert packet == original
	}
}

fn test_protect_header_only_touches_declared_bits() {
	// Header protection for a long header must only ever touch the low 4
	// bits of the first byte (reserved + pn-length) -- never the header
	// form, fixed, or long-header-type bits (top 4 bits), which are always
	// sent in the clear.
	hp_key := rand.bytes(16)!
	mut packet := []u8{}
	packet << u8(0xc0) // Initial, pn_length_bits=0 (i.e. pn_length=1)
	packet << [u8(0xaa), 0xbb, 0xcc, 0xdd]
	pn_offset := packet.len
	packet << u8(0x99)
	packet << []u8{len: 32, init: 0x11}

	protect_header(mut packet, pn_offset, 1, hp_key, .long)!
	assert packet[0] & 0xf0 == 0xc0
}

fn test_protect_header_rejects_invalid_pn_length() {
	hp_key := rand.bytes(16)!
	mut packet := []u8{len: 64}
	protect_header(mut packet, 5, 0, hp_key, .long) or {
		assert err.msg().contains('invalid packet number length')
		return
	}
	assert false, 'expected an error for pn_length 0'
}

fn test_protect_header_rejects_pn_length_over_four() {
	hp_key := rand.bytes(16)!
	mut packet := []u8{len: 64}
	protect_header(mut packet, 5, 5, hp_key, .long) or {
		assert err.msg().contains('invalid packet number length')
		return
	}
	assert false, 'expected an error for pn_length 5'
}

fn test_protect_header_rejects_packet_too_short_to_sample() {
	hp_key := rand.bytes(16)!
	// pn_offset=5, pn_length=2, but only 10 bytes total: not enough room for
	// the mandatory 4+16-byte sample window past pn_offset.
	mut packet := []u8{len: 10}
	protect_header(mut packet, 5, 2, hp_key, .long) or {
		assert err.msg().contains('too short')
		return
	}
	assert false, 'expected an error for a too-short packet'
}

fn test_unprotect_header_rejects_packet_too_short_to_sample() {
	hp_key := rand.bytes(16)!
	mut packet := []u8{len: 10}
	unprotect_header(mut packet, 5, hp_key, .long) or {
		assert err.msg().contains('too short')
		return
	}
	assert false, 'expected an error for a too-short packet'
}

fn test_unprotect_header_rejects_non_zero_reserved_bits_long_header() {
	hp_key := rand.bytes(16)!
	mut packet := []u8{}
	packet << u8(0xc0) // Initial, reserved=00, pn_length_bits=0 (pn_length=1)
	packet << [u8(0xaa), 0xbb, 0xcc, 0xdd]
	pn_offset := packet.len
	packet << u8(0x99)
	packet << []u8{len: 32, init: 0x11}

	protect_header(mut packet, pn_offset, 1, hp_key, .long)!
	// Flip one of the two reserved-bit positions (0x0c) directly on the
	// still-protected byte -- XOR commutes, so this has the same effect as
	// if the ORIGINAL plaintext packet had carried a non-zero reserved bit
	// (RFC 9000 §17.2 requires senders always zero these; a receiver seeing
	// a non-zero value after unmasking must treat it as PROTOCOL_VIOLATION,
	// not accept the packet).
	packet[0] ^= 0x04

	unprotect_header(mut packet, pn_offset, hp_key, .long) or {
		assert err.msg().contains('reserved bits')
		return
	}
	assert false, 'expected non-zero reserved bits to be rejected'
}

fn test_unprotect_header_rejects_non_zero_reserved_bits_short_header() {
	hp_key := rand.bytes(16)!
	dcid := rand.bytes(8)!
	mut packet := []u8{}
	packet << u8(0x40) // short header, reserved=00, key_phase=0, pn_length_bits=0
	packet << dcid
	pn_offset := packet.len
	packet << u8(0x22)
	packet << []u8{len: 32, init: 0x33}

	protect_header(mut packet, pn_offset, 1, hp_key, .short)!
	packet[0] ^= 0x08 // one of the short-header reserved bits (0x18)

	unprotect_header(mut packet, pn_offset, hp_key, .short) or {
		assert err.msg().contains('reserved bits')
		return
	}
	assert false, 'expected non-zero reserved bits to be rejected'
}

fn test_header_protection_mask_rejects_wrong_key_length() {
	mut packet := []u8{len: 64}
	protect_header(mut packet, 0, 1, rand.bytes(32)!, .long) or {
		assert err.msg().contains('16 bytes')
		return
	}
	assert false, 'expected an error for a non-16-byte header protection key'
}
