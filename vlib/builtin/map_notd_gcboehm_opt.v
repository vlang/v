module builtin

// Dummy placeholder for functions from map_d_gcboehm_opt.v
// These functions will be called by cgen, for maps that have keys and values
// of primitive types, that do not require GC scanning, like ints, bools etc.

[inline]
fn new_dense_array_noscan(key_bytes int, key_noscan bool, value_bytes int, value_noscan bool) DenseArray {
	return new_dense_array(key_bytes, value_bytes)
}

[inline]
fn new_map_noscan_key(key_bytes int, value_bytes int, hash_fn MapHashFn, key_eq_fn MapEqFn, clone_fn MapCloneFn, free_fn MapFreeFn) map {
	return new_map(key_bytes, value_bytes, hash_fn, key_eq_fn, clone_fn, free_fn)
}

[inline]
fn new_map_noscan_value(key_bytes int, value_bytes int, hash_fn MapHashFn, key_eq_fn MapEqFn, clone_fn MapCloneFn, free_fn MapFreeFn) map {
	return new_map(key_bytes, value_bytes, hash_fn, key_eq_fn, clone_fn, free_fn)
}

[inline]
fn new_map_noscan_key_value(key_bytes int, value_bytes int, hash_fn MapHashFn, key_eq_fn MapEqFn, clone_fn MapCloneFn, free_fn MapFreeFn) map {
	return new_map(key_bytes, value_bytes, hash_fn, key_eq_fn, clone_fn, free_fn)
}

[inline]
fn new_map_init_noscan_key(hash_fn MapHashFn, key_eq_fn MapEqFn, clone_fn MapCloneFn, free_fn MapFreeFn, n int, key_bytes int, value_bytes int, keys voidptr, values voidptr) map {
	return new_map_init(hash_fn, key_eq_fn, clone_fn, free_fn, n, key_bytes, value_bytes,
		keys, values)
}

[inline]
fn new_map_init_noscan_value(hash_fn MapHashFn, key_eq_fn MapEqFn, clone_fn MapCloneFn, free_fn MapFreeFn, n int, key_bytes int, value_bytes int, keys voidptr, values voidptr) map {
	return new_map_init(hash_fn, key_eq_fn, clone_fn, free_fn, n, key_bytes, value_bytes,
		keys, values)
}

[inline]
fn new_map_init_noscan_key_value(hash_fn MapHashFn, key_eq_fn MapEqFn, clone_fn MapCloneFn, free_fn MapFreeFn, n int, key_bytes int, value_bytes int, keys voidptr, values voidptr) map {
	return new_map_init(hash_fn, key_eq_fn, clone_fn, free_fn, n, key_bytes, value_bytes,
		keys, values)
}
