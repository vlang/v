@[has_globals]
module builtin

__global (
	length_filter_hash_calls  int
	length_filter_eq_calls    int
	length_filter_clone_calls int
)

fn length_filter_custom_hash(key voidptr) u64 {
	length_filter_hash_calls++
	return map_hash_string(key)
}

fn length_filter_custom_eq(a voidptr, b voidptr) bool {
	length_filter_eq_calls++
	return map_eq_string(a, b)
}

fn length_filter_custom_clone(destination voidptr, source voidptr) {
	length_filter_clone_calls++
	map_clone_string(destination, source)
}

fn length_filter_shortening_clone(destination voidptr, source voidptr) {
	length_filter_clone_calls++
	map_clone_string(destination, source)
	unsafe { (&string(destination)).len-- }
}

fn length_filter_first_byte_clone(destination voidptr, source voidptr) {
	length_filter_clone_calls++
	map_clone_string(destination, source)
	unsafe { (&string(destination)).str[0] = u8(255) }
}

fn assert_length_filter_read(values &map, key string, expected int) {
	assert values.exists(&key) == (expected >= 0)
	found := values.get_check(&key)
	if expected >= 0 {
		assert found != unsafe { nil }
		assert unsafe { *&int(found) } == expected
	} else {
		assert found == unsafe { nil }
	}
	zero := -1
	assert unsafe { *&int(values.get(&key, &zero)) } == expected
}

fn new_length_filter_map() map {
	return new_map(int(sizeof(string)), int(sizeof(int)), map_hash_string, map_eq_string,
		map_clone_string, map_free_string)
}

fn test_map_string_length_filter_handles_length_residues_and_clones() {
	$if v3_backend ? {
		mut values := new_length_filter_map()
		keys := ['', 'a'.repeat(31), 'b'.repeat(32), 'c'.repeat(63), 'd'.repeat(64), 'e'.repeat(129),
			'a\x00b', '\x00z', 'é', '界', 'f'.repeat(127), 'g'.repeat(128)]
		for i, key in keys {
			value := i + 10
			values.set(&key, &value)
		}
		assert values.data.eq_kind == 17
		expected_bits := u64(1) | (u64(1) << 31) | (u64(1) << 32) | (u64(1) << 63) |
			(u64(1) << 1) | (u64(1) << 2) | (u64(1) << 3)
		assert values.data.string_length_bits == expected_bits
		expected_first := u64(1) | (u64(1) << 33) | (u64(1) << 34) | (u64(1) << 35) |
			(u64(1) << 36) | (u64(1) << 37) | (u64(1) << 38) | (u64(1) << 3) |
			(u64(1) << 39)
		assert values.data.string_first_bits == expected_first
		missing := 'xxxx'
		assert values.data.rejects_string_key(&missing)
		mut direct := unsafe { values.clone() }
		assert direct.data.string_length_bits == values.data.string_length_bits
		assert direct.data.string_first_bits == values.data.string_first_bits
		values.reserve(4096)
		mut sparse := unsafe { values.clone() }
		assert sparse.data.string_length_bits == values.data.string_length_bits
		assert sparse.data.string_first_bits == values.data.string_first_bits
		for i, key in keys {
			assert_length_filter_read(&values, key, i + 10)
			assert_length_filter_read(&direct, key, i + 10)
			assert_length_filter_read(&sparse, key, i + 10)
		}
		assert_length_filter_read(&values, 'xx', -1)
		assert_length_filter_read(&values, 'z'.repeat(31), -1)
		key := keys[1]
		updated := 77
		bits := values.data.string_length_bits
		first_bits := values.data.string_first_bits
		values.set(&key, &updated)
		assert values.data.string_length_bits == bits
		assert values.data.string_first_bits == first_bits
		assert_length_filter_read(&values, key, updated)
		unsafe {
			direct.free()
			sparse.free()
			values.free()
		}
	}
}

fn test_map_string_length_filter_survives_deletion_compaction_and_insert_on_read() {
	$if v3_backend ? {
		mut values := new_length_filter_map()
		for i in 0 .. 64 {
			key := 'entry-${100 + i}'
			values.set(&key, &i)
		}
		bits := values.data.string_length_bits
		first_bits := values.data.string_first_bits
		for i in 0 .. 32 {
			key := 'entry-${100 + i}'
			unsafe { values.delete(&key) }
		}
		assert values.data.key_values.len == 32
		assert values.data.key_values.deletes == 0
		assert values.data.string_length_bits == bits
		assert values.data.string_first_bits == first_bits
		values.reserve(4096)
		for i in 0 .. 64 {
			assert_length_filter_read(&values, 'entry-${100 + i}', if i >= 32 { i } else { -1 })
		}
		key := 'new-key-longer'
		zero := 99
		assert unsafe { *&int(values.get_and_set(&key, &zero)) } == zero
		assert !values.data.rejects_string_key(&key)
		assert_length_filter_read(&values, key, zero)
		unsafe { values.free() }
	}
}

fn test_map_string_length_filter_saturation_clear_empty_clone_and_move() {
	$if v3_backend ? {
		mut values := new_length_filter_map()
		values.clear()
		assert values.data.eq_kind == 17
		assert values.data.string_length_bits == 0
		assert values.data.string_first_bits == 0
		for i in 0 .. 64 {
			key := 's'.repeat(i)
			values.set(&key, &i)
		}
		assert values.data.string_length_bits == u64(0xffffffffffffffff)
		assert values.data.string_first_bits == u64(1) | (u64(1) << 51)
		assert values.data.eq_kind == 17
		missing := 'z'
		assert values.data.rejects_string_key(&missing)
		assert_length_filter_read(&values, missing, -1)
		for i in 0 .. 64 {
			key := [u8(i), u8(120)].bytestr()
			values.set(&key, &i)
		}
		assert values.data.string_first_bits == u64(0xffffffffffffffff)
		assert values.data.eq_kind == 1
		mut direct := unsafe { values.clone() }
		assert direct.data.string_length_bits == u64(0xffffffffffffffff)
		assert direct.data.string_first_bits == u64(0xffffffffffffffff)
		assert direct.data.eq_kind == 1
		values.reserve(4096)
		mut sparse := unsafe { values.clone() }
		assert sparse.data.string_length_bits == u64(0xffffffffffffffff)
		assert sparse.data.string_first_bits == u64(0xffffffffffffffff)
		assert sparse.data.eq_kind == 1
		mut moved := sparse.move()
		assert sparse.data.eq_kind == 17
		assert sparse.data.string_length_bits == 0
		assert sparse.data.string_first_bits == 0
		for i in 0 .. 64 {
			key := 's'.repeat(i)
			assert_length_filter_read(&direct, key, i)
			assert_length_filter_read(&moved, key, i)
			byte_key := [u8(i), u8(120)].bytestr()
			assert_length_filter_read(&direct, byte_key, i)
			assert_length_filter_read(&moved, byte_key, i)
		}
		values.clear()
		assert values.data.eq_kind == 17
		assert values.data.string_length_bits == 0
		assert values.data.string_first_bits == 0
		mut empty := unsafe { values.clone() }
		assert empty.data.eq_kind == 17
		assert empty.data.string_length_bits == 0
		assert empty.data.string_first_bits == 0
		for i in 1 .. 4 {
			key := 'r'.repeat(i * 64)
			values.set(&key, &i)
		}
		assert values.data.eq_kind == 17
		assert values.data.string_length_bits == 1
		assert values.data.string_first_bits == u64(1) << 50
		assert_length_filter_read(&values, 'r'.repeat(128), 2)
		assert_length_filter_read(&values, 's'.repeat(31), -1)
		unsafe {
			direct.free()
			moved.free()
			sparse.free()
			empty.free()
			values.free()
		}
	}
}

fn test_map_string_length_filter_keeps_length_filter_when_first_is_saturated() {
	$if v3_backend ? {
		mut values := new_length_filter_map()
		for i in 0 .. 64 {
			key := [u8(i), u8(120)].bytestr()
			values.set(&key, &i)
		}
		assert values.data.string_length_bits == u64(1) << 2
		assert values.data.string_first_bits == u64(0xffffffffffffffff)
		assert values.data.eq_kind == 17
		missing := 'abc'
		assert values.data.rejects_string_key(&missing)
		assert_length_filter_read(&values, missing, -1)
		for i in 0 .. 64 {
			key := [u8(i), u8(120)].bytestr()
			assert_length_filter_read(&values, key, i)
		}
		unsafe { values.free() }
	}
}

fn test_map_string_length_filter_handles_unsigned_first_byte_residues() {
	$if v3_backend ? {
		mut values := new_length_filter_map()
		first_bytes := [u8(0), 63, 64, 127, 128, 255]
		for i, first in first_bytes {
			key := [first, u8(i), u8(120)].bytestr()
			values.set(&key, &i)
		}
		assert values.data.string_length_bits == u64(1) << 3
		assert values.data.string_first_bits == u64(1) | (u64(1) << 63)
		missing := [u8(1), u8(0), u8(120)].bytestr()
		assert values.data.rejects_string_key(&missing)
		assert_length_filter_read(&values, missing, -1)
		for i, first in first_bytes {
			key := [first, u8(i), u8(120)].bytestr()
			assert_length_filter_read(&values, key, i)
		}
		unsafe { values.free() }
	}
}

fn test_map_string_length_filter_compact_clone_rebuilds_saturated_history() {
	$if v3_backend ? {
		mut values := new_length_filter_map()
		for i in 0 .. 64 {
			key := 's'.repeat(i)
			values.set(&key, &i)
			byte_key := [u8(i), u8(120)].bytestr()
			values.set(&byte_key, &i)
		}
		assert values.data.eq_kind == 1
		for i in 0 .. 64 {
			byte_key := [u8(i), u8(120)].bytestr()
			unsafe { values.delete(&byte_key) }
			if i < 61 {
				key := 's'.repeat(i)
				unsafe { values.delete(&key) }
			}
		}
		assert values.data.count == 3
		assert values.data.string_length_bits == u64(0xffffffffffffffff)
		assert values.data.string_first_bits == u64(0xffffffffffffffff)
		values.reserve(4096)
		mut compact := unsafe { values.clone() }
		assert compact.data.eq_kind == 17
		assert compact.data.string_length_bits == (u64(1) << 61) | (u64(1) << 62) |
			(u64(1) << 63)
		assert compact.data.string_first_bits == u64(1) << 51
		for i in 61 .. 64 {
			assert_length_filter_read(&compact, 's'.repeat(i), i)
		}
		missing := 'x'.repeat(61)
		assert compact.data.rejects_string_key(&missing)
		assert_length_filter_read(&compact, missing, -1)
		unsafe {
			compact.free()
			values.free()
		}
	}
}

fn test_map_string_length_filter_keeps_custom_callbacks_and_clone_lengths() {
	$if v3_backend ? {
		for mode in 1 .. 8 {
			hash_fn := if mode & 1 != 0 { length_filter_custom_hash } else { map_hash_string }
			eq_fn := if mode & 2 != 0 { length_filter_custom_eq } else { map_eq_string }
			clone_fn := if mode & 4 != 0 { length_filter_custom_clone } else { map_clone_string }
			mut values := new_map(int(sizeof(string)), int(sizeof(int)), hash_fn, eq_fn,
				clone_fn, map_free_string)
			length_filter_clone_calls = 0
			for i in 1 .. 5 {
				key := 'k'.repeat(i)
				values.set(&key, &i)
			}
			assert values.data.eq_kind & 16 == 0
			assert values.data.string_length_bits == 0
			assert values.data.string_first_bits == 0
			assert length_filter_clone_calls == if mode & 4 != 0 { 4 } else { 0 }
			length_filter_hash_calls = 0
			length_filter_eq_calls = 0
			assert_length_filter_read(&values, 'kkkk', 4)
			assert length_filter_hash_calls == if mode & 1 != 0 { 3 } else { 0 }
			assert length_filter_eq_calls == if mode & 2 != 0 { 3 } else { 0 }
			assert_length_filter_read(&values, 'missing-length', -1)
			assert length_filter_hash_calls == if mode & 1 != 0 { 6 } else { 0 }
			values.clear()
			assert values.data.eq_kind & 16 == 0
			unsafe { values.free() }
		}
		mut shortened := new_map(int(sizeof(string)), int(sizeof(int)), map_hash_string,
			map_eq_string, length_filter_shortening_clone, map_free_string)
		length_filter_clone_calls = 0
		for i in 1 .. 5 {
			key := 'x'.repeat(i + 1)
			shortened.set(&key, &i)
		}
		assert length_filter_clone_calls == 4
		assert shortened.data.eq_kind == 1
		assert shortened.data.string_length_bits == 0
		assert shortened.data.string_first_bits == 0
		for i in 0 .. 4 {
			assert unsafe { (&string(shortened.data.key_values.key(i))).len } == i + 1
		}
		// This raw callback changes key identity; original keys must retain ordinary misses.
		assert_length_filter_read(&shortened, 'xxxxx', -1)
		unsafe { shortened.free() }
		mut changed_first := new_map(int(sizeof(string)), int(sizeof(int)), map_hash_string,
			map_eq_string, length_filter_first_byte_clone, map_free_string)
		length_filter_clone_calls = 0
		for i in 1 .. 5 {
			key := 'x'.repeat(i + 1)
			changed_first.set(&key, &i)
		}
		assert length_filter_clone_calls == 4
		assert changed_first.data.eq_kind == 1
		assert changed_first.data.string_length_bits == 0
		assert changed_first.data.string_first_bits == 0
		for i in 0 .. 4 {
			assert unsafe { (&string(changed_first.data.key_values.key(i))).str[0] } == u8(255)
		}
		assert_length_filter_read(&changed_first, 'xxxxx', -1)
		unsafe { changed_first.free() }
	}
}

fn test_map_string_length_filter_seeds_only_live_provided_dense_keys() {
	$if v3_backend ? {
		mut dense := new_dense_array(int(sizeof(string)), int(sizeof(int)), 0, 0)
		keys := ['', 'discard', 'x'.repeat(31), 'y'.repeat(32)]
		for i, key in keys {
			index := dense.expand()
			map_clone_string(dense.key(index), &key)
			unsafe { vmemcpy(dense.value(index), &i, int(sizeof(int))) }
		}
		map_free_string(dense.key(1))
		dense.delete(1)
		metas := unsafe { &u32(vcalloc_noscan(int(sizeof(u32) * (init_capicity + extra_metas_inc)))) }
		mut values := new_map_with_dense_array(int(sizeof(string)), int(sizeof(int)),
			map_hash_string, map_eq_string, map_clone_string, map_free_string, dense, metas)
		expected_bits := u64(1) | (u64(1) << 31) | (u64(1) << 32)
		assert values.data.string_length_bits == expected_bits
		assert values.data.string_first_bits == u64(1) | (u64(1) << 56) | (u64(1) << 57)
		assert values.data.rejects_string_key(&keys[1])
		values.data.count = 3
		values.data.rehash()
		for i, key in keys {
			assert_length_filter_read(&values, key, if i == 1 { -1 } else { i })
		}
		unsafe { values.free() }
	}
}

fn length_filter_read_worker(values &map, keys []string, worker_id int) int {
	$if v3_backend ? {
		missing := 'absent-length'
		zero := -1
		for iteration in 0 .. 256 {
			index := (iteration + worker_id) % keys.len
			key := keys[index]
			found := values.get_check(&key)
			if found == unsafe { nil } || unsafe { *&int(found) } != index + 1
				|| !values.exists(&key)
				|| unsafe { *&int(values.get(&key, &zero)) } != index + 1 {
				return -1
			}
			if values.get_check(&missing) != unsafe { nil } || values.exists(&missing)
				|| values.get(&missing, &zero) != voidptr(&zero) {
				return -1
			}
		}
	}
	return worker_id
}

fn test_map_string_length_filter_allows_concurrent_readonly_mixed_keys() {
	$if v3_backend ? {
		mut values := new_length_filter_map()
		keys := ['alpha', 'b'.repeat(31), 'c'.repeat(32), '\x00'.repeat(64)]
		for i, key in keys {
			value := i + 1
			values.set(&key, &value)
		}
		bits := values.data.string_length_bits
		first_bits := values.data.string_first_bits
		mut threads := []thread int{}
		for worker_id in 0 .. 4 {
			threads << spawn length_filter_read_worker(&values, keys, worker_id)
		}
		for worker_id, handle in threads {
			assert handle.wait() == worker_id
		}
		assert values.data.string_length_bits == bits
		assert values.data.string_first_bits == first_bits
		// All readers have joined before changing or releasing the map.
		values.clear()
		unsafe { values.free() }
	}
}
