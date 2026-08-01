module quic

// decode_from_bytes is a small test helper: parse a big-endian byte slice
// (as produced by encode_packet_number) back into its numeric value, the same
// way a real header parser would after removing header protection.
fn bytes_to_u64(b []u8) u64 {
	mut v := u64(0)
	for x in b {
		v = (v << 8) | u64(x)
	}
	return v
}

fn test_packet_number_first_in_space_returns_truncated_as_is() {
	// With no prior packet number processed in this space, there is no
	// "largest" to reconstruct against — the truncated value is the value.
	decoded := decode_packet_number(42, 4, none)!
	assert decoded == 42
}

fn test_packet_number_encode_length_selection() {
	// RFC 9000 §17.1: the encoding must represent more than twice the gap
	// between the packet being sent and the largest acknowledged packet.
	// full_pn - largest_acked = 1 (gap 0..1 window) -> fits in 1 byte.
	_, len1 := encode_packet_number(101, u64(100))!
	assert len1 == 1

	// A gap large enough to need the 2-byte class.
	_, len2 := encode_packet_number(100 + 200, u64(100))!
	assert len2 == 2

	// A gap large enough to need the 3-byte class.
	_, len3 := encode_packet_number(100 + 100000, u64(100))!
	assert len3 == 3

	// No prior ack at all: full 4-byte encoding.
	_, len4 := encode_packet_number(5, none)!
	assert len4 == 4
}

fn test_packet_number_round_trip_across_length_boundaries() {
	// (largest_acked, full_pn) pairs chosen to exercise each length class.
	cases := [
		[u64(0), u64(1)],
		[u64(100), u64(101)],
		[u64(1000), u64(1300)],
		[u64(1_000_000), u64(1_150_000)],
		[u64(100_000_000), u64(140_000_000)],
	]
	for c in cases {
		largest_acked := c[0]
		full_pn := c[1]
		encoded, n := encode_packet_number(full_pn, largest_acked)!
		assert encoded.len == n
		truncated := bytes_to_u64(encoded)
		decoded := decode_packet_number(truncated, n, largest_acked)!
		assert decoded == full_pn, 'largest_acked=${largest_acked} full_pn=${full_pn}: got ${decoded}'
	}
}

fn test_packet_number_reordered_arrival_reconstructs_correctly() {
	// A packet sent before the current largest-processed one (reordered
	// delivery) must still reconstruct to its own, smaller, correct value —
	// not be forced upward to look larger than largest_pn.
	largest_pn := u64(1000)
	earlier_full_pn := u64(990)
	encoded, n := encode_packet_number(earlier_full_pn, u64(950))!
	truncated := bytes_to_u64(encoded)
	decoded := decode_packet_number(truncated, n, largest_pn)!
	assert decoded == earlier_full_pn
}

// test_packet_number_encode_rejects_exhausted_number is a regression test
// for a Codex finding (vlang/v#27680 pullrequestreview-4791164664):
// encode_packet_number was infallible and silently truncated any full_pn,
// including values at or above 2^62 (RFC 9000 §12.3's packet number space
// limit) -- 2^62 truncates to a 4-byte field of all zeros instead of being
// rejected, corrupting the packet instead of telling the caller this space
// is exhausted.
fn test_packet_number_encode_rejects_exhausted_number() {
	encode_packet_number(max_packet_number + 1, none) or {
		assert err.msg().contains('exceeds the maximum')
		return
	}
	assert false, 'expected an error for a packet number above 2^62-1'
}

// Uses a concrete largest_acked (not none) so this test exercises only the
// full_pn > max_packet_number exhaustion check (this function's very first
// guard), not the separate "no ack yet, does full_pn fit in 4 bytes" check
// added for pullrequestreview-4822597219 -- max_packet_number (2^62-1)
// itself does not fit in 4 bytes, so with largest_acked=none this would
// now be (correctly) rejected by THAT check instead, which is not what
// this test is about. This test previously passed `none` here and got
// away with it only because the "no ack yet" branch didn't validate
// full_pn's magnitude at all before this round's fix -- see
// test_packet_number_encode_rejects_oversized_first_packet_in_space below.
fn test_packet_number_encode_accepts_boundary_max_packet_number() {
	largest_acked := max_packet_number - ((u64(1) << 31) - 1)
	_, n := encode_packet_number(max_packet_number, largest_acked)!
	assert n == 4
}

// test_packet_number_encode_rejects_gap_too_large_for_four_bytes is a
// regression test for a Codex finding (vlang/v#27680
// pullrequestreview-4806500473): the length-selection `match` had no upper
// bound on its `else { 4 }` arm, so a gap too large even for a 4-byte
// encoding (more than 2^32) fell through to 4 bytes anyway instead of being
// rejected. Confirmed via a standalone repro before this fix:
// encode_packet_number(max_packet_number, Some(0)) silently produced 4
// bytes that decode_packet_number then reconstructed as 2^32-1, a DIFFERENT
// value than the 2^62-1 actually sent.
fn test_packet_number_encode_rejects_gap_too_large_for_four_bytes() {
	encode_packet_number(max_packet_number, u64(0)) or {
		assert err.msg().contains('exceeds what a 4-byte encoding')
		return
	}
	assert false, 'expected an error for a packet-number gap too large for any encoding length'
}

// test_packet_number_encode_accepts_boundary_four_byte_gap is the same
// boundary check as test_packet_number_encode_accepts_boundary_max_packet_number
// but pinned to the gap arithmetic itself: (num_unacked+1)*2 == 2^32 exactly
// must still succeed (4 bytes) -- only a gap that EXCEEDS this must be
// rejected, not one that exactly reaches it.
fn test_packet_number_encode_accepts_boundary_four_byte_gap() {
	largest_acked := u64(0)
	// num_unacked = 2^31-1, so (num_unacked+1)*2 = 2^32 exactly -- the
	// boundary the new check must accept, not just anything strictly below it.
	full_pn := (u64(1) << 31) - 1
	encoded, n := encode_packet_number(full_pn, largest_acked)!
	assert n == 4
	truncated := bytes_to_u64(encoded)
	decoded := decode_packet_number(truncated, n, largest_acked)!
	assert decoded == full_pn
}

// test_packet_number_encode_rejects_oversized_first_packet_in_space is a
// regression test for a gap the previous fix (the gap-check above) didn't
// close: when largest_acked is none, num_unacked was a fixed sentinel
// (0x7FFF_FFFF) rather than something derived from full_pn, so the gap check
// above could never fire in this branch regardless of how large full_pn
// actually was. A full_pn that doesn't fit in 4 bytes silently truncated to
// its low 32 bits instead of erroring (Codex P2, vlang/v#27680
// pullrequestreview-4822597219): encode_packet_number(0x1_0000_0000, none)
// returned four zero bytes, and decode_packet_number then reconstructed 0,
// not 2^32.
fn test_packet_number_encode_rejects_oversized_first_packet_in_space() {
	encode_packet_number(u64(0x1_0000_0000), none) or {
		assert err.msg().contains('cannot be represented by the 4-byte encoding')
		return
	}
	assert false, 'expected an error for a first-in-space packet number that does not fit in 4 bytes'
}

// test_packet_number_encode_accepts_boundary_first_packet_in_space pins the
// exact boundary: 2^32-1 (0xFFFF_FFFF) is the largest value 4 bytes CAN
// represent and must still succeed, not just anything strictly below it.
fn test_packet_number_encode_accepts_boundary_first_packet_in_space() {
	full_pn := u64(0xFFFF_FFFF)
	encoded, n := encode_packet_number(full_pn, none)!
	assert n == 4
	truncated := bytes_to_u64(encoded)
	decoded := decode_packet_number(truncated, n, none)!
	assert decoded == full_pn
}

fn test_packet_number_invalid_length_rejected() {
	decode_packet_number(1, 5, none) or {
		assert err.msg().contains('invalid packet number length')
		return
	}
	assert false, 'expected an error for pn_len outside 1-4'
	decode_packet_number(1, 0, none) or {
		assert err.msg().contains('invalid packet number length')
		return
	}
}
