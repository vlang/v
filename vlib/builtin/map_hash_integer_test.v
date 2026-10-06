module builtin

fn test_integer_hash_mixes_high_bits_with_every_c_compiler() {
	keys := [u64(0), 1, u64(1) << 32, u64(1) << 44, u64(1) << 63, u64(0xffffffffffffffff)]
	hashes := [u64(0xca813bf4c7abf0a9), 0x5ed9c758b9c48de0, 0x70f0cf3c53f32535, 0xa4569cb05af20b0a,
		0x0ca561be9c542e04, 0xd111bbf2944bfa09]
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
