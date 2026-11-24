module builtin

fn dense_for_compaction_test(m &map) &DenseArray {
	$for field in map.fields {
		$if field.name == 'data' {
			return &m.data.key_values
		}
		$if field.name == 'key_values' {
			return &m.key_values
		}
	}
	panic('map has no dense storage')
}

fn check_compacted_keys(d &DenseArray, expected []u16) {
	mut next := 0
	for i in 0 .. d.len {
		key := unsafe { *(&u16(d.key(i))) }
		if d.has_index(i) {
			assert key == expected[next]
			next++
		} else {
			assert key == 0
		}
	}
	assert next == expected.len
}

fn test_grown_map_reclaims_dense_storage_across_partial_deletion_and_repopulation() {
	// Specify a zero-byte value explicitly; C backends can give an empty V
	// struct a one-byte representation. The one-byte value sentinel must survive.
	count := 4097
	mut m := new_map(2, 0, map_hash_int_2, map_eq_int_2, map_clone_int_2, map_free_nop)
	defer { unsafe { m.free() } }
	mut order := []u16{cap: count}
	value := u8(0)
	for i in 0 .. count {
		key := u16(i)
		m.set(&key, &value)
		order << key
	}
	d := dense_for_compaction_test(&m)
	initial_cap := d.cap
	for cycle in 0 .. 8 {
		start_cap := d.cap
		mut kept := []u16{cap: count}
		mut removed := []u16{cap: count}
		for key in order {
			if (int(key) + cycle) % 4 == 0 {
				kept << key
			} else {
				unsafe { m.delete(&key) }
				removed << key
			}
		}
		assert d.cap < start_cap
		assert d.cap >= kept.len
		check_compacted_keys(d, kept)
		for i in d.len .. d.cap {
			assert unsafe { *(&u16(d.key(i))) } == 0
		}
		for i in 0 .. count {
			key := u16(i)
			assert m.exists(&key) == ((i + cycle) % 4 == 0)
		}
		for i := removed.len - 1; i >= 0; i-- {
			key := removed[i]
			m.set(&key, &value)
			kept << key
		}
		check_compacted_keys(d, kept)
		assert d.cap <= 2 * initial_cap
		order = kept.clone()
	}
}
