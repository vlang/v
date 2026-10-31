module builtin

fn map_shift_test_hash(key voidptr) u64 {
	return unsafe { *&u64(key) }
}

fn test_map_hash_shift_mask_preserves_all_in_range_shifts() {
	$if v3_backend ? {
		mut state := VMapData{
			even_index: 0x7ffffffe
			hash_fn:    map_shift_test_hash
			metas:      unsafe { nil }
		}
		for shift in 0 .. 64 {
			state.shift = u8(shift)
			for key in [u64(0), 1, 0x8000000000000000, 0xffffffffffffffff, 0x123456789abcdef0] {
				index, meta := state.key_to_index(&key)
				assert index == u32(key & 0x7ffffffe)
				assert meta == u32(((key >> shift) & hash_mask) | probe_inc)
			}
			mut key := u64(0x9e3779b97f4a7c15)
			for _ in 0 .. 1024 {
				// Xorshift visits mixed high and low bits without overflowing arithmetic.
				key ^= key << 13
				key ^= key >> 7
				key ^= key << 17
				index, meta := state.key_to_index(&key)
				assert index == u32(key & 0x7ffffffe)
				assert meta == u32(((key >> shift) & hash_mask) | probe_inc)
			}
		}
	}
}

fn test_map_hash_shift_stays_within_hash_until_index_exhausted() {
	// Model the growth recurrence without allocating exponentially large tables.
	mut even_index := u64(init_even_index)
	mut cached := max_cached_hashbits
	mut shift := init_log_capicity
	mut expansions := 0
	for {
		assert even_index <= 0xfffffffe
		assert shift == init_log_capicity || shift == init_log_capicity + max_cached_hashbits
		assert shift < 64
		next := ((even_index + 2) << 1) - 2
		if next > 0xfffffffe {
			break
		}
		even_index = next
		if cached == 0 {
			shift += max_cached_hashbits
			cached = max_cached_hashbits
		} else {
			cached--
		}
		expansions++
	}
	assert expansions == 27
	assert shift == 21
}
