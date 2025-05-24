module stdatomic

// Implement the atomic operations. For now TCC does not support the atomic
// versions on nix so it uses locks to simulate the same behavior.
//
// On windows tcc can simulate with other atomic operations.
//
// Note: this implementations should be regarded as alpha stage and be tested
// much more.

// add_u64 adds provided delta as an atomic operation
@[inline]
pub fn add_u64(ptr &u64, delta int) u64 {
	C.atomic_fetch_add_u64(voidptr(ptr), delta)
	return *ptr
}

// sub_u64 subtracts provided delta as an atomic operation
@[inline]
pub fn sub_u64(ptr &u64, delta int) u64 {
	C.atomic_fetch_sub_u64(voidptr(ptr), delta)
	return *ptr
}

// add_i64 adds provided delta as an atomic operation
@[inline]
pub fn add_i64(ptr &i64, delta int) i64 {
	C.atomic_fetch_add_u64(voidptr(ptr), delta)
	return *ptr
}

// add_i64 subtracts provided delta as an atomic operation
@[inline]
pub fn sub_i64(ptr &i64, delta int) i64 {
	C.atomic_fetch_sub_u64(voidptr(ptr), delta)
	return *ptr
}

// atomic store/load operations have to be used when there might be another concurrent access
// atomicall set a value
@[inline]
pub fn store_u64(ptr &u64, val u64) {
	C.atomic_store_u64(voidptr(ptr), val)
}

// atomicall get a value
@[inline]
pub fn load_u64(ptr &u64) u64 {
	return C.atomic_load_u64(voidptr(ptr))
}

// atomicall set a value
@[inline]
pub fn store_i64(ptr &i64, val i64) {
	C.atomic_store_u64(voidptr(ptr), val)
}

// atomicall get a value
@[inline]
pub fn load_i64(ptr &i64) i64 {
	return i64(C.atomic_load_u64(voidptr(ptr)))
}

@[heap]
pub struct AtomicVal[T] {
	val T
}

// new_atomic creates a new atomic value of `T` type
@[inline]
pub fn new_atomic[T](val T) &AtomicVal[T] {
	$if T is $int || T is bool {
		return &AtomicVal[T]{
			val: val
		}
	} $else {
		panic('atomic: only support number and bool types')
	}
	return unsafe { nil }
}

// load returns current value of the atomic value
@[inline]
pub fn (mut a AtomicVal[T]) load() T {
	$if T is bool {
		return C.atomic_load_byte(voidptr(&a.val)) != 0
	} $else $if T is u8 || T is i8 {
		return T(C.atomic_load_byte(voidptr(&a.val)))
	} $else $if T is u16 || T is i16 {
		return T(C.atomic_load_u16(voidptr(&a.val)))
	} $else $if T is u32 || T is i32 {
		return T(C.atomic_load_u32(voidptr(&a.val)))
	} $else $if T is u64 || T is i64 {
		return T(C.atomic_load_u64(voidptr(&a.val)))
	} $else $if T is int {
		// TODO: remove this test or a compile time support $if sizeof() ==
		if sizeof(int) == 4 {
			return int(C.atomic_load_u32(voidptr(&a.val)))
		} else {
			return int(C.atomic_load_u64(voidptr(&a.val)))
		}
	} $else $if T is isize || T is usize {
		// TODO: remove this test or a compile time support $if sizeof() ==
		if sizeof(isize) == 4 {
			return T(C.atomic_load_u32(voidptr(&a.val)))
		} else {
			return T(C.atomic_load_u64(voidptr(&a.val)))
		}
	}
	return a.val
}

// store updates the atomic value with `val`
@[inline]
pub fn (mut a AtomicVal[T]) store(val T) {
	$if T is bool {
		C.atomic_store_byte(voidptr(&a.val), val)
	} $else $if T is u8 || T is i8 {
		C.atomic_store_byte(voidptr(&a.val), val)
	} $else $if T is u16 || T is i16 {
		C.atomic_store_u16(voidptr(&a.val), val)
	} $else $if T is u32 || T is i32 {
		C.atomic_store_u32(voidptr(&a.val), val)
	} $else $if T is u64 || T is i64 {
		C.atomic_store_u64(voidptr(&a.val), val)
	} $else $if T is int {
		// TODO: remove this test or a compile time support $if sizeof() ==
		if sizeof(int) == 4 {
			C.atomic_store_u32(voidptr(&a.val), val)
		} else {
			C.atomic_store_u64(voidptr(&a.val), val)
		}
	} $else $if T is isize || T is usize {
		// TODO: remove this test or a compile time support $if sizeof() ==
		if sizeof(isize) == 4 {
			C.atomic_store_u32(voidptr(&a.val), val)
		} else {
			C.atomic_store_u64(voidptr(&a.val), val)
		}
	}
}

// add adds the atomic value with `delta`
@[inline]
pub fn (mut a AtomicVal[T]) add(delta T) T {
	$if T is bool {
		panic('atomic: can not add() a bool type')
	} $else $if T is u8 || T is i8 {
		C.atomic_fetch_add_byte(voidptr(&a.val), delta)
	} $else $if T is u16 || T is i16 {
		C.atomic_fetch_add_u16(voidptr(&a.val), delta)
	} $else $if T is u32 || T is i32 {
		C.atomic_fetch_add_u32(voidptr(&a.val), delta)
	} $else $if T is u64 || T is i64 {
		C.atomic_fetch_add_u64(voidptr(&a.val), delta)
	} $else $if T is int {
		// TODO: remove this test or a compile time support $if sizeof() ==
		if sizeof(int) == 4 {
			C.atomic_fetch_add_u32(voidptr(&a.val), delta)
		} else {
			C.atomic_fetch_add_u64(voidptr(&a.val), delta)
		}
	} $else $if T is isize || T is usize {
		// TODO: remove this test or a compile time support $if sizeof() ==
		if sizeof(isize) == 4 {
			C.atomic_fetch_add_u32(voidptr(&a.val), delta)
		} else {
			C.atomic_fetch_add_u64(voidptr(&a.val), delta)
		}
	}
	return T(a.val)
}

// sub subs the atomic value with `delta`
@[inline]
pub fn (mut a AtomicVal[T]) sub(delta T) T {
	$if T is bool {
		panic('atomic: can not sub() a bool type')
	} $else $if T is u8 || T is i8 {
		C.atomic_fetch_sub_byte(voidptr(&a.val), delta)
	} $else $if T is u16 || T is i16 {
		C.atomic_fetch_sub_u16(voidptr(&a.val), delta)
	} $else $if T is u32 || T is i32 {
		C.atomic_fetch_sub_u32(voidptr(&a.val), delta)
	} $else $if T is u64 || T is i64 {
		C.atomic_fetch_sub_u64(voidptr(&a.val), delta)
	} $else $if T is int {
		// TODO: remove this test or a compile time support $if sizeof() ==
		if sizeof(int) == 4 {
			C.atomic_fetch_sub_u32(voidptr(&a.val), delta)
		} else {
			C.atomic_fetch_sub_u64(voidptr(&a.val), delta)
		}
	} $else $if T is isize || T is usize {
		// TODO: remove this test or a compile time support $if sizeof() ==
		if sizeof(isize) == 4 {
			C.atomic_fetch_sub_u32(voidptr(&a.val), delta)
		} else {
			C.atomic_fetch_sub_u64(voidptr(&a.val), delta)
		}
	}
	return T(a.val)
}
