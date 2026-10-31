module builtin

fn string_length_hash(key voidptr) u64 {
	return u64(unsafe { (&string(key)).len })
}

fn string_length_equal(a voidptr, b voidptr) bool {
	return unsafe { (&string(a)).len == (&string(b)).len }
}

fn test_map_custom_callbacks_are_preserved() {
	// Equal lengths intentionally identify different strings as the same key.
	mut values := new_map(int(sizeof(string)), int(sizeof(int)), string_length_hash,
		string_length_equal, map_clone_string, map_free_string)
	for i in 1 .. 12 {
		key := 'a'.repeat(i)
		value := i * 10
		values.set(&key, &value)
	}
	for i in 1 .. 12 {
		key := 'b'.repeat(i)
		found := values.get_check(&key)
		assert found != unsafe { nil }
		assert unsafe { *&int(found) } == i * 10
		assert values.exists(&key)
		zero := -1
		assert unsafe { *&int(values.get(&key, &zero)) } == i * 10
		value := i * 100
		values.set(&key, &value)
		assert unsafe { *&int(values.get_check(&key)) } == value
	}
	for i in 1 .. 12 {
		key := 'c'.repeat(i)
		values.delete(&key)
		assert !values.exists(&key)
		assert values.get_check(&key) == unsafe { nil }
	}
}

fn integer_mod_hash(key voidptr) u64 {
	return u64(unsafe { *&u64(key) } % 16)
}

fn integer_mod_equal(a voidptr, b voidptr) bool {
	return unsafe { *&u64(a) % 16 == *&u64(b) % 16 }
}

fn test_map_custom_integer_callbacks_are_preserved() {
	mut values := new_map(int(sizeof(u64)), int(sizeof(int)), integer_mod_hash,
		integer_mod_equal, map_clone_int_8, map_free_nop)
	for i in 0 .. 12 {
		key := u64(i)
		values.set(&key, &i)
		// Exercise the one-entry and two-entry paths before the normal hash path.
		for j in 0 .. i + 1 {
			equivalent := u64(j + 16)
			found := values.get_check(&equivalent)
			assert found != unsafe { nil }
			assert unsafe { *&int(found) } == j
			assert values.exists(&equivalent)
			stored := values.get_key_check(&equivalent)
			assert stored != unsafe { nil }
			assert unsafe { *&u64(stored) } == u64(j)
		}
	}
	for i in 0 .. 12 {
		key := u64(i + 32)
		value := i * 10
		values.set(&key, &value)
		assert unsafe { *&int(values.get_check(&key)) } == value
		values.delete(&key)
		assert !values.exists(&key)
	}
}

fn map_probe_cluster_hash(key voidptr) u64 {
	// Colliding home buckets exercise both matching and unequal probe metadata.
	return u64(unsafe { *&u64(key) } % 8)
}

fn test_map_read_probes_terminate_across_collisions_and_deleted_slots() {
	mut values := new_map(int(sizeof(u64)), int(sizeof(int)), map_probe_cluster_hash,
		map_eq_int_8, map_clone_int_8, map_free_nop)
	for i in 0 .. 128 {
		key := u64(i)
		value := i + 10
		values.set(&key, &value)
	}
	for i in 0 .. 128 {
		if i % 3 == 0 {
			key := u64(i)
			values.delete(&key)
		}
	}
	zero := -1
	for i in 0 .. 256 {
		key := u64(i)
		present := i < 128 && i % 3 != 0
		assert values.exists(&key) == present
		found := values.get_check(&key)
		stored := values.get_key_check(&key)
		if present {
			assert !isnil(found)
			assert !isnil(stored)
			assert unsafe { *&int(found) } == i + 10
			assert unsafe { *&u64(stored) } == key
			assert unsafe { *&int(values.get(&key, &zero)) } == i + 10
		} else {
			assert isnil(found)
			assert isnil(stored)
			assert values.get(&key, &zero) == voidptr(&zero)
		}
	}
}
