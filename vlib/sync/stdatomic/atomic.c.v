module stdatomic

// Implement the atomic operations. For now TCC does not support the atomic
// versions on nix so it uses locks to simulate the same behavor.
//
// On windows tcc can simulate with other atomic operations.
//
// Note: this implementations should be regarded as alpha stage and be tested
// much more.

// add_u64 adds provided delta as an atomic operation
pub fn add_u64(ptr &u64, delta int) u64 {
	C.atomic_fetch_add_u64(voidptr(ptr), delta)
	return *ptr
}

// sub_u64 subtracts provided delta as an atomic operation
pub fn sub_u64(ptr &u64, delta int) u64 {
	C.atomic_fetch_sub_u64(voidptr(ptr), delta)
	return *ptr
}

// add_i64 adds provided delta as an atomic operation
pub fn add_i64(ptr &i64, delta int) i64 {
	C.atomic_fetch_add_u64(voidptr(ptr), delta)
	return *ptr
}

// add_i64 subtracts provided delta as an atomic operation
pub fn sub_i64(ptr &i64, delta int) i64 {
	C.atomic_fetch_sub_u64(voidptr(ptr), delta)
	return *ptr
}

// atomic store/load operations have to be used when there might be another concurrent access
// atomicall set a value
pub fn store_u64(ptr &u64, val u64) {
	C.atomic_store_u64(voidptr(ptr), val)
}

// atomicall get a value
pub fn load_u64(ptr &u64) u64 {
	return C.atomic_load_u64(voidptr(ptr))
}

// atomicall set a value
pub fn store_i64(ptr &i64, val i64) {
	C.atomic_store_u64(voidptr(ptr), val)
}

// atomicall get a value
pub fn load_i64(ptr &i64) i64 {
	return i64(C.atomic_load_u64(voidptr(ptr)))
}
