module builtin

// map_hash_bytes reads a key of up to 16 bytes through four words that overlap,
// and the end of a longer key through two: every byte still has to reach the hash.
fn test_map_hash_bytes_depends_on_every_byte() {
	for len in 1 .. 130 {
		mut key := []u8{len: len, init: u8(index * 7 + 3)}
		base := unsafe { map_hash_bytes(key.data, len) }
		for pos in 0 .. len {
			for bit in [u8(0x01), 0x10, 0x80] {
				key[pos] ^= bit
				changed := unsafe { map_hash_bytes(key.data, len) }
				key[pos] ^= bit
				assert changed != base, 'byte ${pos} of a key of ${len} bytes does not change its hash'
			}
		}
	}
}

// The hash of a key does not depend on the C compiler: the 128-bit product of
// `_wymix` and the four 64-bit ones that replace it give the same words.
fn test_map_hash_bytes_is_the_same_with_every_c_compiler() {
	keys := ['', 'a', 'abc', 'abcdefgh', 'main.Point', 'sixteen bytes key',
		'a key that is longer than forty-eight bytes, to take the three products side by side']
	hashes := [u64(0x8ea3afbe99066b15), 0x1fc84f1de36a9945, 0xd2363222da46d433, 0x138853f748152981,
		0xd2269a0833d751ee, 0x66f8e599c83350ad, 0xbede46ac99786947]
	for i, key in keys {
		assert unsafe { map_hash_bytes(key.str, key.len) } == hashes[i], key
	}
}

fn test_map_hash_bytes_depends_on_the_length() {
	zeros := []u8{len: 200}
	mut seen := map[u64]int{}
	for len in 0 .. 200 {
		hash := unsafe { map_hash_bytes(zeros.data, len) }
		assert hash !in seen, 'runs of ${seen[hash]} and of ${len} zero bytes hash alike'
		seen[hash] = len
	}
}

// A map takes the slot of a key from the low bits of its hash, and the tag that it
// probes with from the high ones.
fn test_map_hash_bytes_spreads_keys_that_differ_in_a_counter() {
	keys := 256_000
	mut low := []int{len: 256}
	mut high := []int{len: 256}
	mut hashes := map[u64]bool{}
	for i in 0 .. keys {
		key := 'module.Struct${i}.field'
		hash := unsafe { map_hash_bytes(key.str, key.len) }
		low[int(hash & 0xff)]++
		high[int(hash >> 56)]++
		hashes[hash] = true
	}
	assert hashes.len == keys
	expected := keys / 256
	for count in low {
		assert count > expected * 8 / 10 && count < expected * 12 / 10
	}
	for count in high {
		assert count > expected * 8 / 10 && count < expected * 12 / 10
	}
}

fn test_string_keys_of_every_length_are_found_again() {
	mut m := map[string]int{}
	mut keys := []string{}
	for len in 0 .. 150 {
		keys << 'k'.repeat(len)
		keys << 'k'.repeat(len) + 'x'
		keys << 'y' + 'k'.repeat(len)
	}
	for i, key in keys {
		m[key] = i
	}
	// A key that the three shapes spell alike is written more than once: the last
	// write wins.
	for key in keys {
		mut last := -1
		for j, other in keys {
			if other == key {
				last = j
			}
		}
		assert m[key] == last
		assert key in m
	}
	assert 'missing' !in m
}
