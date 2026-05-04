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
