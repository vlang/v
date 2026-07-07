module quic

// QUIC packet number encoding (RFC 9000 §17.1) and decoding/reconstruction
// (RFC 9000 Appendix A). Packet numbers are truncated on the wire to the
// minimum number of bytes (1-4) that still lets the receiver reconstruct the
// full value unambiguously, given the largest packet number it has already
// acknowledged in the same packet number space.
//
// NOTE: packet number spaces (Initial/Handshake/1-RTT) are independent of
// each other — encoding/decoding here always operates within a single space;
// callers must track `largest_acked`/`largest_pn` per space (see
// packet_number_space.v, added in a later phase), never as one connection-wide
// value.

// encode_packet_number picks the smallest encoding (1, 2, 3, or 4 bytes) for
// `full_pn` such that it can be unambiguously reconstructed given
// `largest_acked` (the largest packet number acknowledged so far in this
// space, or none if nothing has been acknowledged yet). Returns the truncated
// bytes and the number of bytes used.
//
// Per RFC 9000 §17.1, the sender MUST use a packet number encoding that can
// represent more than twice as large a range as the difference between the
// packet number being sent and the largest acknowledged packet.
pub fn encode_packet_number(full_pn u64, largest_acked ?u64) ([]u8, int) {
	// If nothing has been acknowledged yet, use the full 4-byte encoding —
	// there's no acked packet number to bound the ambiguity window against.
	num_unacked := if la := largest_acked {
		if full_pn > la { full_pn - la } else { u64(0) }
	} else {
		u64(0x7FFF_FFFF) // force the 4-byte path below
	}

	n := match true {
		(num_unacked + 1) * 2 <= 0x100 { 1 }
		(num_unacked + 1) * 2 <= 0x1_0000 { 2 }
		(num_unacked + 1) * 2 <= 0x100_0000 { 3 }
		else { 4 }
	}

	mut out := []u8{len: n}
	for i in 0 .. n {
		shift := u32((n - 1 - i) * 8)
		out[i] = u8(full_pn >> shift)
	}
	return out, n
}

// decode_packet_number reconstructs the full packet number from its
// on-the-wire truncated form, given the largest packet number successfully
// processed so far in this space (RFC 9000 Appendix A.3, `DecodePacketNumber`).
// `truncated` must already be the numeric value of the `pn_len`-byte field
// (i.e. the header-protection mask must already have been removed and the
// bytes parsed as a big-endian integer of that length).
pub fn decode_packet_number(truncated u64, pn_len int, largest_pn ?u64) !u64 {
	if pn_len < 1 || pn_len > 4 {
		return error('invalid packet number length ${pn_len}, must be 1-4')
	}
	pn_nbits := u32(pn_len * 8)
	pn_win := u64(1) << pn_nbits
	pn_hwin := pn_win / 2

	largest := largest_pn or {
		// No packet processed yet in this space: the truncated value already
		// is the full value (first packet is always sent with a full-width
		// encoding by a spec-compliant sender, but decoding is defined
		// regardless of sender behavior).
		return truncated
	}

	pn_mask := pn_win - 1
	expected_pn := largest + 1
	candidate_pn := (expected_pn & ~pn_mask) | truncated

	// `expected_pn - pn_hwin` and `candidate_pn - pn_win` would underflow
	// (wrap to a huge u64) if computed unconditionally when the minuend is
	// smaller than the subtrahend — guard both explicitly rather than
	// relying on unsigned wraparound to "happen to" fail the comparison.
	below_lower_bound := expected_pn >= pn_hwin && candidate_pn <= expected_pn - pn_hwin
	fits_after_adding_window := candidate_pn < (u64(1) << 62) - pn_win

	above_upper_bound := candidate_pn > expected_pn + pn_hwin
	fits_after_subtracting_window := candidate_pn >= pn_win

	return if below_lower_bound && fits_after_adding_window {
		candidate_pn + pn_win
	} else if above_upper_bound && fits_after_subtracting_window {
		candidate_pn - pn_win
	} else {
		candidate_pn
	}
}
