module builtin

// Preserve the previous four-half-word formulation independently of the new
// word-load shortcut, including its different midpoint at exactly 16 bytes.
fn previous_short_map_hash(key &u8, len int) u64 {
	mut a := u64(0)
	mut b := u64(0)
	unsafe {
		if len >= 4 {
			mid := (len >> 3) << 2
			a = (map_hash_half(key) << 32) | map_hash_half(key + mid)
			b = (map_hash_half(key + len - 4) << 32) | map_hash_half(key + len - 4 - mid)
		} else if len > 0 {
			a = (u64(key[0]) << 16) | (u64(key[len >> 1]) << 8) | u64(key[len - 1])
		}
	}
	return C._wymix(C._wymix(a ^ map_hash_k1, b ^ map_hash_k0) ^ map_hash_k0 ^ u64(len),
		map_hash_k1)
}

fn test_map_hash_bytes_preserves_short_hash_at_every_length_and_alignment() {
	for offset in 0 .. 16 {
		for len in 0 .. 17 {
			bytes := []u8{len: offset + len + 16, init: u8(index * 73 + len * 19)}
			key := unsafe { &u8(bytes.data) + offset }
			expected := previous_short_map_hash(key, len)
			assert unsafe { map_hash_bytes(key, len) } == expected, 'length ${len}, offset ${offset}'
		}
	}
}
