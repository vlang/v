module builtin

// A plain byte-at-a-time statement of the map hash, independent of the word
// loads of `map_hash_bytes`: the two must agree at every length and alignment.
fn reference_map_hash(key &u8, len int) u64 {
	mut seed := map_hash_k0 ^ (u64(len) * map_hash_k2)
	if len <= 16 {
		mut a := u64(0)
		mut b := u64(0)
		if len >= 8 {
			a = reference_word(key, 0, 8)
			b = reference_word(key, len - 8, 8)
		} else if len >= 4 {
			a = reference_word(key, 0, 4)
			b = reference_word(key, len - 4, 4)
		} else if len > 0 {
			a = unsafe { (u64(key[0]) << 16) | (u64(key[len >> 1]) << 8) | u64(key[len - 1]) }
		}
		return C._wymix(a ^ map_hash_k1, b ^ seed)
	}
	mut start := 0
	for len - start > 16 {
		seed = C._wymix(reference_word(key, start, 8) ^ map_hash_k1, reference_word(key,
			start + 8, 8) ^ seed)
		start += 16
	}
	return C._wymix(reference_word(key, len - 16, 8) ^ map_hash_k1, reference_word(key,
		len - 8, 8) ^ seed)
}

fn reference_word(key &u8, start int, bytes int) u64 {
	mut word := u64(0)
	for i in 0 .. bytes {
		word |= u64(unsafe { key[start + i] }) << (8 * i)
	}
	return word
}

fn test_map_hash_bytes_matches_the_reference_at_every_length_and_alignment() {
	for offset in 0 .. 16 {
		for len in 0 .. 70 {
			bytes := []u8{len: offset + len + 16, init: u8(index * 73 + len * 19)}
			key := unsafe { &u8(bytes.data) + offset }
			expected := reference_map_hash(key, len)
			assert unsafe { map_hash_bytes(key, len) } == expected, 'length ${len}, offset ${offset}'
		}
	}
}
