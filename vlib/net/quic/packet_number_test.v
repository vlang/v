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
	_, len1 := encode_packet_number(101, u64(100))
	assert len1 == 1

	// A gap large enough to need the 2-byte class.
	_, len2 := encode_packet_number(100 + 200, u64(100))
	assert len2 == 2

	// A gap large enough to need the 3-byte class.
	_, len3 := encode_packet_number(100 + 100000, u64(100))
	assert len3 == 3

	// No prior ack at all: full 4-byte encoding.
	_, len4 := encode_packet_number(5, none)
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
		encoded, n := encode_packet_number(full_pn, largest_acked)
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
	encoded, n := encode_packet_number(earlier_full_pn, u64(950))
	truncated := bytes_to_u64(encoded)
	decoded := decode_packet_number(truncated, n, largest_pn)!
	assert decoded == earlier_full_pn
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
