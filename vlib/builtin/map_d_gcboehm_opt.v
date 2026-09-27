// "noscan" versions of `map` initialization routines
//
// They are used when the compiler can proof that either the keys or the values or both
// do not contain any pointers. Such objects can be placed in a memory area that is not
// scanned by the garbage collector

module builtin

@[inline]
fn __malloc_at_least_one(how_many_bytes u64, noscan bool) &u8 {
	if noscan {
		return unsafe { malloc_noscan(__at_least_one(how_many_bytes)) }
	}
	return unsafe { malloc(__at_least_one(how_many_bytes)) }
}

@[inline]
fn new_dense_array_noscan(key_bytes int, key_noscan bool, value_bytes int, value_noscan bool) DenseArray {
	cap := 8
	return DenseArray{
		key_bytes:   key_bytes
		value_bytes: value_bytes
		cap:         cap
		len:         0
		deletes:     0
		all_deleted: unsafe { nil }
		keys:        __malloc_at_least_one(u64(cap) * u64(key_bytes), key_noscan)
		values:      __malloc_at_least_one(u64(cap) * u64(value_bytes), value_noscan)
	}
}

fn new_map_noscan_key(key_bytes int, value_bytes int, hash_fn MapHashFn, key_eq_fn MapEqFn, clone_fn MapCloneFn,
	free_fn MapFreeFn) map {
	metasize := int(sizeof(u32) * (init_capicity + extra_metas_inc))
	return new_map_with_dense_array(key_bytes, value_bytes, hash_fn, key_eq_fn, clone_fn,
		free_fn, new_dense_array_noscan(key_bytes, true, value_bytes, false),
		unsafe { &u32(vcalloc_noscan(metasize)) })
}

fn new_map_noscan_value(key_bytes int, value_bytes int, hash_fn MapHashFn, key_eq_fn MapEqFn, clone_fn MapCloneFn,
	free_fn MapFreeFn) map {
	metasize := int(sizeof(u32) * (init_capicity + extra_metas_inc))
	return new_map_with_dense_array(key_bytes, value_bytes, hash_fn, key_eq_fn, clone_fn,
		free_fn, new_dense_array_noscan(key_bytes, false, value_bytes, true),
		unsafe { &u32(vcalloc_noscan(metasize)) })
}

fn new_map_noscan_key_value(key_bytes int, value_bytes int, hash_fn MapHashFn, key_eq_fn MapEqFn, clone_fn MapCloneFn,
	free_fn MapFreeFn) map {
	metasize := int(sizeof(u32) * (init_capicity + extra_metas_inc))
	return new_map_with_dense_array(key_bytes, value_bytes, hash_fn, key_eq_fn, clone_fn,
		free_fn, new_dense_array_noscan(key_bytes, true, value_bytes, true),
		unsafe { &u32(vcalloc_noscan(metasize)) })
}

fn new_map_init_noscan_key(hash_fn MapHashFn, key_eq_fn MapEqFn, clone_fn MapCloneFn, free_fn MapFreeFn,
	n int, key_bytes int, value_bytes int, keys voidptr, values voidptr) map {
	mut out := new_map_noscan_key(key_bytes, value_bytes, hash_fn, key_eq_fn, clone_fn, free_fn)
	// TODO: pre-allocate n slots
	mut pkey := &u8(keys)
	mut pval := &u8(values)
	for _ in 0 .. n {
		unsafe {
			out.set(pkey, pval)
			pkey = pkey + key_bytes
			pval = pval + value_bytes
		}
	}
	return out
}

fn new_map_init_noscan_value(hash_fn MapHashFn, key_eq_fn MapEqFn, clone_fn MapCloneFn, free_fn MapFreeFn,
	n int, key_bytes int, value_bytes int, keys voidptr, values voidptr) map {
	mut out := new_map_noscan_value(key_bytes, value_bytes, hash_fn, key_eq_fn, clone_fn, free_fn)
	// TODO: pre-allocate n slots
	mut pkey := &u8(keys)
	mut pval := &u8(values)
	for _ in 0 .. n {
		unsafe {
			out.set(pkey, pval)
			pkey = pkey + key_bytes
			pval = pval + value_bytes
		}
	}
	return out
}

fn new_map_init_noscan_key_value(hash_fn MapHashFn, key_eq_fn MapEqFn, clone_fn MapCloneFn, free_fn MapFreeFn,
	n int, key_bytes int, value_bytes int, keys voidptr, values voidptr) map {
	mut out := new_map_noscan_key_value(key_bytes, value_bytes, hash_fn, key_eq_fn, clone_fn,
		free_fn)
	// TODO: pre-allocate n slots
	mut pkey := &u8(keys)
	mut pval := &u8(values)
	for _ in 0 .. n {
		unsafe {
			out.set(pkey, pval)
			pkey = pkey + key_bytes
			pval = pval + value_bytes
		}
	}
	return out
}
