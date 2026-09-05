struct DenseArrayLayoutForTest {
	key_bytes   int
	value_bytes int
mut:
	cap         int
	len         int
	deletes     u32
	all_deleted &u8 = unsafe { nil }
	keys        &u8 = unsafe { nil }
	values      &u8 = unsafe { nil }
}

struct MapLayoutForTest {
	key_bytes   int
	value_bytes int
mut:
	even_index      u32
	cached_hashbits u8
	shift           u8
	key_values      DenseArrayLayoutForTest
	metas           &u32 = unsafe { nil }
	extra_metas     u32
	has_string_keys bool
	hash_fn         voidptr
	key_eq_fn       voidptr
	clone_fn        voidptr
	free_fn         voidptr
pub mut:
	len int
}

fn test_map_delete_reclaims_dense_array_tail() {
	mut m := map[string]string{}
	raw := unsafe { &MapLayoutForTest(&m) }
	// Establish backing storage before checking repeated insertion/deletion.
	m['first'] = 'first'
	m.delete('first')
	initial_cap := raw.key_values.cap
	for i in 0 .. 100 {
		key := i.str()
		m[key] = key
		m.delete(key)
		assert m.len == 0
		assert raw.key_values.len == 0
		assert raw.key_values.deletes == 0
		assert raw.key_values.cap == initial_cap
		assert raw.key_values.all_deleted == unsafe { nil }
	}
}

fn test_empty_map_defers_storage_until_first_write() {
	mut m := map[string]int{}
	raw := unsafe { &MapLayoutForTest(&m) }
	assert raw.metas == unsafe { nil }
	assert raw.key_values.keys == unsafe { nil }
	assert raw.key_values.values == unsafe { nil }
	assert m['absent'] == 0
	assert 'absent' !in m
	assert m.keys().len == 0
	assert m.values().len == 0
	m.delete('absent')
	m.clear()
	m.reserve(0)
	mut copy := m.clone()
	assert raw.metas == unsafe { nil }
	copy['copy'] += 2
	assert copy == {
		'copy': 2
	}
	assert m.len == 0
	m['original']++
	assert m == {
		'original': 1
	}
	assert copy == {
		'copy': 2
	}
	assert raw.metas != unsafe { nil }
	unsafe { copy.free() }
	m.clear()
	m['reused'] = 3
	assert m == {
		'reused': 3
	}
	unsafe { m.free() }
	mut empty := map[string]int{}
	unsafe { empty.free() }
}

fn test_lazy_map_small_reservations_and_nested_first_writes() {
	for n in [0, 1, 2, 7, 8, 9, 100] {
		mut m := map[int]int{}
		m.reserve(u32(n))
		for i in 0 .. 100 {
			m[i] += i + 1
		}
		for i in 0 .. 100 {
			assert m[i] == i + 1
		}
		mut copy := m.clone()
		m.clear()
		assert copy.len == 100
		copy[0]++
		assert copy[0] == 2
		assert m.len == 0
	}
	mut arrays := map[string][]int{}
	arrays['new'] << 1
	arrays['new'] << 2
	assert arrays['new'] == [1, 2]
	mut nested := map[string]map[string]int{}
	nested['outer']['inner'] = 1
	assert nested['outer']['inner'] == 1
}

fn test_map_reserve_preallocates_dense_array() {
	mut m := map[string]int{}
	raw := unsafe { &MapLayoutForTest(&m) }
	m.reserve(1000)
	assert raw.key_values.cap >= 1000
	keys := raw.key_values.keys
	values := raw.key_values.values
	for i in 0 .. 1000 {
		m[i.str()] = i
	}
	assert raw.key_values.keys == keys
	assert raw.key_values.values == values
}

fn test_empty_map_reserve_matches_populated_map_hash_state() {
	// The largest reservation crosses the cached-hash-bit rollover.
	for n in [32, 1024, 1_000_000] {
		mut empty := map[int]int{}
		mut populated := {
			-1: 42
		}
		empty.reserve(u32(n))
		populated.reserve(u32(n))
		e := unsafe { &MapLayoutForTest(&empty) }
		p := unsafe { &MapLayoutForTest(&populated) }
		assert e.even_index == p.even_index
		assert e.cached_hashbits == p.cached_hashbits
		assert e.shift == p.shift
		reserved_index := e.even_index
		for i in 0 .. n {
			empty[i] = i * 3
		}
		// Collision tails may grow without rehashing the reserved buckets.
		assert e.even_index == reserved_index
		for i in 0 .. n {
			assert empty[i] == i * 3
		}
		if n < 1_000_000 {
			for i in n .. n * 2 {
				empty[i] = i * 3
			}
			for i in 0 .. n * 2 {
				assert empty[i] == i * 3
			}
		}
		unsafe {
			empty.free()
			populated.free()
		}
	}
}

fn test_empty_map_reserve_after_deletion_and_clear() {
	mut m := {
		'old': 1
	}
	m.delete('old')
	m.reserve(1024)
	m['new'] = 2
	assert m == {
		'new': 2
	}
	m.clear()
	m.reserve(2048)
	m['cleared'] = 3
	assert m == {
		'cleared': 3
	}
	m.reserve(0)
	m['next'] = 4
	assert m.keys() == ['cleared', 'next']
}
