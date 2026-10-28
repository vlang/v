@[has_globals]
module builtin

__global (
	callback_kind_hash_calls int
	callback_kind_eq_calls   int
)

fn callback_kind_custom_hash(key voidptr) u64 {
	callback_kind_hash_calls++
	return map_hash_int_8(key) ^ u64(0xa0761d6478bd642f)
}

fn callback_kind_custom_eq(a voidptr, b voidptr) bool {
	callback_kind_eq_calls++
	return map_eq_int_8(a, b)
}

fn check_callback_kind_tags(values &map, hash_kind u8, eq_kind u8) {
	$if v3_backend ? {
		assert values.data.hash_kind == hash_kind
		assert values.data.eq_kind == eq_kind
	}
}

fn check_callback_kind_reads(values &map, count int) {
	for i in 0 .. count + 1 {
		key := u64(i) << 40
		assert values.exists(&key) == (i < count)
		found := values.get_check(&key)
		zero := -1
		if i < count {
			assert found != unsafe { nil }
			assert unsafe { *&int(found) } == i + 10
			assert unsafe { *&int(values.get(&key, &zero)) } == i + 10
		} else {
			assert found == unsafe { nil }
			assert values.get(&key, &zero) == voidptr(&zero)
		}
	}
}

fn test_map_callback_kinds_preserve_independent_custom_callbacks_and_clones() {
	for mode in 0 .. 4 {
		hash_fn := if mode & 1 != 0 { callback_kind_custom_hash } else { map_hash_int_8 }
		eq_fn := if mode & 2 != 0 { callback_kind_custom_eq } else { map_eq_int_8 }
		mut values := new_map(int(sizeof(u64)), int(sizeof(int)), hash_fn, eq_fn,
			map_clone_int_8, map_free_nop)
		hash_kind := if mode & 1 != 0 { u8(0) } else { u8(8) }
		eq_kind := if mode & 2 != 0 { u8(0) } else { u8(8) }
		check_callback_kind_tags(&values, hash_kind, eq_kind)
		mut empty := unsafe { values.clone() }
		check_callback_kind_tags(&empty, hash_kind, eq_kind)
		check_callback_kind_reads(&empty, 0)
		for i in 0 .. 32 {
			key := u64(i) << 40
			value := i + 10
			values.set(&key, &value)
			check_callback_kind_reads(&values, i + 1)
		}
		callback_kind_hash_calls = 0
		callback_kind_eq_calls = 0
		check_callback_kind_reads(&values, 32)
		assert (callback_kind_hash_calls > 0) == (mode & 1 != 0)
		assert (callback_kind_eq_calls > 0) == (mode & 2 != 0)
		mut dense := unsafe { values.clone() }
		check_callback_kind_reads(&dense, 32)
		check_callback_kind_tags(&dense, hash_kind, eq_kind)
		values.reserve(4096)
		mut sparse := unsafe { values.clone() }
		check_callback_kind_reads(&sparse, 32)
		check_callback_kind_tags(&sparse, hash_kind, eq_kind)
		values.clear()
		check_callback_kind_tags(&values, hash_kind, eq_kind)
		mut moved := sparse.move()
		check_callback_kind_reads(&moved, 32)
		check_callback_kind_reads(&sparse, 0)
		check_callback_kind_tags(&sparse, hash_kind, eq_kind)
		unsafe {
			empty.free()
			dense.free()
			moved.free()
			sparse.free()
			values.free()
		}
	}
}

fn test_map_callback_kinds_cover_standard_constructors_and_header_reset() {
	mut strings := map[string]int{}
	strings['one'] = 1
	strings['two'] = 2
	strings['three'] = 3
	assert strings['three'] == 3
	mut small := map[u32]int{}
	for i in 0 .. 32 {
		small[u32(i) << 24] = i
	}
	for i in 0 .. 32 {
		assert small[u32(i) << 24] == i
	}
	$if v3_backend ? {
		assert strings.data.hash_kind == 1
		assert strings.data.eq_kind & 15 == 1
		assert small.data.hash_kind == 4
		assert small.data.eq_kind == 4
	}
	key := u64(1) << 40
	value := 42
	$if v3_backend ? {
		mut data := new_map_data(int(sizeof(u64)), int(sizeof(int)), map_hash_int_8,
			map_eq_int_8, map_clone_int_8, map_free_nop)
		assert data.hash_kind == 8
		assert data.eq_kind == 8
		data.set(&key, &value)
		unsafe { data.free() }
		assert data.hash_kind == 0
		assert data.eq_kind == 0
		unsafe { free(data) }
	}
	mut dense := new_map_with_dense_array(int(sizeof(u64)), int(sizeof(int)),
		map_hash_int_8, callback_kind_custom_eq, map_clone_int_8, map_free_nop,
		DenseArray{ key_bytes: int(sizeof(u64)), value_bytes: int(sizeof(int)) }, unsafe { nil })
	check_callback_kind_tags(&dense, 8, 0)
	dense.set(&key, &value)
	assert dense.exists(&key)
	assert unsafe { *&int(dense.get_check(&key)) } == value
	unsafe {
		strings.free()
		small.free()
		dense.free()
	}
}
