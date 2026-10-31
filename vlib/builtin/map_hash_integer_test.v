module builtin

fn test_integer_hash_mixes_high_bits_with_every_c_compiler() {
	keys := [u64(0), 1, u64(1) << 32, u64(1) << 44, u64(1) << 63, u64(0xffffffffffffffff)]
	hashes := [u64(0xbc956e8ecb2e6e19), 0x030f406543ebc24d, 0x25d2c2d96b9881fd, 0x8a487489aaf8d7a5,
		0x4c52593c8f813c32, 0xbbadee89d92e665d]
	for i, key in keys {
		assert map_hash_int_8(&key) == hashes[i]
	}
	mut buckets := []int{len: 256}
	for i in u64(0) .. 65536 {
		key := i << 44
		buckets[int(map_hash_int_8(&key) & 255)]++
	}
	for count in buckets {
		assert count > 128 && count < 512
	}
}

fn test_map_u64_keys_that_differ_only_in_high_bits() {
	mut values := map[u64]int{}
	for i in u64(0) .. 200000 {
		values[i << 44] = int(i)
	}
	assert values.len == 200000
	for i in u64(0) .. 200000 {
		assert values[i << 44] == int(i)
		assert i << 44 in values
	}
	for i in u64(0) .. 200000 {
		if i % 2 == 0 {
			values.delete(i << 44)
		}
	}
	assert values.len == 100000
	for i in u64(0) .. 200000 {
		assert (i << 44 in values) == (i % 2 != 0)
	}
}

fn test_map_f64_keys_with_many_zero_low_bits() {
	mut values := map[f64]int{}
	for i in 0 .. 200000 {
		values[f64(i)] = i
	}
	assert values.len == 200000
	for i in 0 .. 200000 {
		assert values[f64(i)] == i
		assert f64(i) in values
	}
}

fn test_integer_hash_keeps_both_halves_when_one_half_matches_the_seed() {
	for fixed_low in [false, true] {
		mut buckets := []int{len: 256}
		for i in u64(0) .. 65536 {
			key := if fixed_low {
				[u64(0x2d358dccaa6c78a5), i]!
			} else {
				[i, u64(0x8bb84b93962eacc9)]!
			}
			buckets[int(map_hash_int_16(&key) & 255)]++
		}
		for count in buckets {
			assert count > 128 && count < 512
		}
	}
}

fn test_map_u128_keys_with_one_half_matching_the_hash_seed() {
	for fixed_low in [false, true] {
		mut values := map[u128]int{}
		for i in 0 .. 2048 {
			key := hash_seed_map_key(fixed_low, i)
			values[key] = i
		}
		assert values.len == 2048
		for i in 0 .. 2048 {
			key := hash_seed_map_key(fixed_low, i)
			assert key in values
			assert values[key] == i
			if i % 2 == 0 {
				values.delete(key)
			}
		}
		assert values.len == 1024
		for i in 0 .. 2048 {
			key := hash_seed_map_key(fixed_low, i)
			assert (key in values) == (i % 2 != 0)
		}
	}
}

fn hash_seed_map_key(fixed_low bool, i int) u128 {
	return if fixed_low {
		(u128(i) << 64) | u128(0x2d358dccaa6c78a5)
	} else {
		(u128(0x8bb84b93962eacc9) << 64) | u128(i)
	}
}
