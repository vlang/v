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

// max_packet_number is RFC 9000 §12.3's own stated limit: "if any packet
// number is exhausted, ... MUST close the connection" -- packet numbers are
// 62-bit unsigned values, so 2^62-1 is the largest one a sender may ever
// use; a sender reaching this limit must stop sending in this space
// (typically by closing the connection or migrating), not keep counting.
pub const max_packet_number = (u64(1) << 62) - 1

// encode_packet_number picks the smallest encoding (1, 2, 3, or 4 bytes) for
// `full_pn` such that it can be unambiguously reconstructed given
// `largest_acked` (the largest packet number acknowledged so far in this
// space, or none if nothing has been acknowledged yet). Returns the truncated
// bytes and the number of bytes used.
//
// Per RFC 9000 §17.1, the sender MUST use a packet number encoding that can
// represent more than twice as large a range as the difference between the
// packet number being sent and the largest acknowledged packet.
//
// Rejects `full_pn > max_packet_number` (RFC 9000 §12.3) rather than
// silently truncating it -- an un-checked caller could otherwise reach here
// with an exhausted counter and get back a corrupted, wrapped-around
// encoding (e.g. 2^62 truncates to a 4-byte field of all zeros) instead of
// the caller finding out its connection needs to stop sending. Also guards
// the `(num_unacked + 1) * 2` sizing arithmetic below, which would itself
// overflow for `num_unacked` values near `u64` max.
pub fn encode_packet_number(full_pn u64, largest_acked ?u64) !([]u8, int) {
	if full_pn > max_packet_number {
		return error('quic: packet number ${full_pn} exceeds the maximum ${max_packet_number} (2^62-1, RFC 9000 §12.3): this packet number space is exhausted')
	}
	// If nothing has been acknowledged yet, use the full 4-byte encoding —
	// there's no acked packet number to bound the ambiguity window against.
	num_unacked := u64(if la := largest_acked {
		if full_pn > la { full_pn - la } else { u64(0) }
	} else {
		// decode_packet_number (below) has no reference point to
		// reconstruct against when largest_pn is none -- it returns the
		// wire bytes AS the full packet number verbatim, with no window
		// arithmetic. Round-tripping correctly therefore requires the
		// chosen n-byte encoding to represent full_pn EXACTLY, not merely
		// "unambiguously" (the twice-the-gap guarantee the acked branch
		// above relies on doesn't apply here at all). This design always
		// picks the 4-byte encoding in this branch, which is only valid
		// when full_pn itself fits in 4 bytes -- checked explicitly,
		// since the sentinel below makes the (num_unacked+1)*2 gap check
		// further down a fixed constant that can never catch this
		// (0x7FFF_FFFF is not derived from full_pn, so
		// (0x7FFF_FFFF+1)*2 == 0x1_0000_0000 always, regardless of how
		// large full_pn actually is).
		if full_pn > 0xFFFF_FFFF {
			return error('quic: packet number ${full_pn} cannot be represented by the 4-byte encoding used for the first packet in a space (no packet acknowledged yet); this exceeds 2^32-1')
		}
		u64(0x7FFF_FFFF) // force the 4-byte path below
	})

	// A gap the 4-byte encoding can't unambiguously represent falls through
	// every arm below with no bound left to check -- silently picking 4
	// bytes anyway truncates full_pn to its low 32 bits, which
	// decode_packet_number then reconstructs as a DIFFERENT packet number
	// than the one actually sent (confirmed: encode_packet_number(2^62-1,
	// Some(0)) round-trips to 2^32-1). Reject rather than silently corrupt.
	if (num_unacked + 1) * 2 > 0x1_0000_0000 {
		return error('quic: packet number gap of ${num_unacked} (full_pn=${full_pn}) exceeds what a 4-byte encoding can unambiguously represent (RFC 9000 §17.1); an ACK updating largest_acked is needed before this packet number can be sent')
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
