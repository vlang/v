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
	$if T is $int {
		return &AtomicVal[T]{
			val: val
		}
	} $else $if T is bool {
		return &AtomicVal[T]{
			val: val
		}
	} $else $if T is voidptr {
		return &AtomicVal[T]{
			val: val
		}
	} $else {
		$compile_error('atomic: only support number, bool, and voidptr types')
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
		if sizeof(int) == 4 {
			return int(C.atomic_load_u32(voidptr(&a.val)))
		} else {
			return int(C.atomic_load_u64(voidptr(&a.val)))
		}
	} $else $if T is isize || T is usize {
		if sizeof(isize) == 4 {
			return T(C.atomic_load_u32(voidptr(&a.val)))
		} else {
			return T(C.atomic_load_u64(voidptr(&a.val)))
		}
	} $else $if T is voidptr {
		// TODO: this should be $if sizeof(T) == 4
		$if x32 {
			return T(C.atomic_load_u32(voidptr(&a.val)))
		} $else {
			return T(C.atomic_load_u64(voidptr(&a.val)))
		}
	}
	return a.val
}

// store updates the atomic value with `val`
@[inline]
pub fn (mut a AtomicVal[T]) store(val T) {
	$if T is bool {
		C.atomic_store_byte(voidptr(&a.val), u8(val))
	} $else $if T is u8 || T is i8 {
		C.atomic_store_byte(voidptr(&a.val), u8(val))
	} $else $if T is u16 || T is i16 {
		C.atomic_store_u16(voidptr(&a.val), u16(val))
	} $else $if T is u32 || T is i32 {
		C.atomic_store_u32(voidptr(&a.val), u32(val))
	} $else $if T is u64 || T is i64 {
		C.atomic_store_u64(voidptr(&a.val), u64(val))
	} $else $if T is int {
		if sizeof(int) == 4 {
			C.atomic_store_u32(voidptr(&a.val), u32(val))
		} else {
			C.atomic_store_u64(voidptr(&a.val), u64(val))
		}
	} $else $if T is isize || T is usize {
		if sizeof(isize) == 4 {
			C.atomic_store_u32(voidptr(&a.val), u32(val))
		} else {
			C.atomic_store_u64(voidptr(&a.val), u64(val))
		}
	} $else $if T is voidptr {
		// TODO: this should be $if sizeof(T) == 4
		$if x32 {
			C.atomic_store_u32(voidptr(&a.val), u32(val))
		} $else {
			C.atomic_store_u64(voidptr(&a.val), u64(val))
		}
	}
}

// add adds the atomic value with `delta` and returns the previous value
@[inline]
pub fn (mut a AtomicVal[T]) add(delta T) T {
	$if T is bool {
		panic('atomic: add() not supported for bool type')
	} $else $if T is voidptr {
		panic('atomic: add() not supported for voidptr type')
	} $else $if T is u8 || T is i8 {
		old := C.atomic_fetch_add_byte(voidptr(&a.val), u8(delta))
		return T(old)
	} $else $if T is u16 || T is i16 {
		old := C.atomic_fetch_add_u16(voidptr(&a.val), u16(delta))
		return T(old)
	} $else $if T is u32 || T is i32 {
		old := C.atomic_fetch_add_u32(voidptr(&a.val), u32(delta))
		return T(old)
	} $else $if T is u64 || T is i64 {
		old := C.atomic_fetch_add_u64(voidptr(&a.val), u64(delta))
		return T(old)
	} $else $if T is int {
		if sizeof(int) == 4 {
			old := C.atomic_fetch_add_u32(voidptr(&a.val), u32(delta))
			return T(old)
		} else {
			old := C.atomic_fetch_add_u64(voidptr(&a.val), u64(delta))
			return T(old)
		}
	} $else $if T is isize || T is usize {
		if sizeof(isize) == 4 {
			old := C.atomic_fetch_add_u32(voidptr(&a.val), u32(delta))
			return T(old)
		} else {
			old := C.atomic_fetch_add_u64(voidptr(&a.val), u64(delta))
			return T(old)
		}
	}
	panic('unreachable')
}

// sub subtracts the atomic value with `delta` and returns the previous value
@[inline]
pub fn (mut a AtomicVal[T]) sub(delta T) T {
	$if T is bool {
		panic('atomic: sub() not supported for bool type')
	} $else $if T is voidptr {
		panic('atomic: sub() not supported for voidptr type')
	} $else $if T is u8 || T is i8 {
		old := C.atomic_fetch_sub_byte(voidptr(&a.val), u8(delta))
		return T(old)
	} $else $if T is u16 || T is i16 {
		old := C.atomic_fetch_sub_u16(voidptr(&a.val), u16(delta))
		return T(old)
	} $else $if T is u32 || T is i32 {
		old := C.atomic_fetch_sub_u32(voidptr(&a.val), u32(delta))
		return T(old)
	} $else $if T is u64 || T is i64 {
		old := C.atomic_fetch_sub_u64(voidptr(&a.val), u64(delta))
		return T(old)
	} $else $if T is int {
		if sizeof(int) == 4 {
			old := C.atomic_fetch_sub_u32(voidptr(&a.val), u32(delta))
			return T(old)
		} else {
			old := C.atomic_fetch_sub_u64(voidptr(&a.val), u64(delta))
			return T(old)
		}
	} $else $if T is isize || T is usize {
		if sizeof(isize) == 4 {
			old := C.atomic_fetch_sub_u32(voidptr(&a.val), u32(delta))
			return T(old)
		} else {
			old := C.atomic_fetch_sub_u64(voidptr(&a.val), u64(delta))
			return T(old)
		}
	}
	panic('unreachable')
}

// swap sets the `new` value and returns the previous value
@[inline]
pub fn (mut a AtomicVal[T]) swap(new T) T {
	$if T is bool {
		old := C.atomic_exchange_byte(voidptr(&a.val), u8(new))
		return old != 0
	} $else $if T is u8 || T is i8 {
		old := C.atomic_exchange_byte(voidptr(&a.val), u8(new))
		return T(old)
	} $else $if T is u16 || T is i16 {
		old := C.atomic_exchange_u16(voidptr(&a.val), u16(new))
		return T(old)
	} $else $if T is u32 || T is i32 {
		old := C.atomic_exchange_u32(voidptr(&a.val), u32(new))
		return T(old)
	} $else $if T is u64 || T is i64 {
		old := C.atomic_exchange_u64(voidptr(&a.val), u64(new))
		return T(old)
	} $else $if T is int {
		if sizeof(int) == 4 {
			old := C.atomic_exchange_u32(voidptr(&a.val), u32(new))
			return T(old)
		} else {
			old := C.atomic_exchange_u64(voidptr(&a.val), u64(new))
			return T(old)
		}
	} $else $if T is isize || T is usize {
		if sizeof(isize) == 4 {
			old := C.atomic_exchange_u32(voidptr(&a.val), u32(new))
			return T(old)
		} else {
			old := C.atomic_exchange_u64(voidptr(&a.val), u64(new))
			return T(old)
		}
	} $else $if T is voidptr {
		// TODO: this should be $if sizeof(T) == 4
		$if x32 {
			old := C.atomic_exchange_u32(voidptr(&a.val), u32(new))
			return T(old)
		} $else {
			old := C.atomic_exchange_u64(voidptr(&a.val), u64(new))
			return T(old)
		}
	}
	panic('unreachable')
}

// compare_and_swap executes the compare-and-swap(CAS) operation
// if atomic value == `expected`, then it will be set to `new`, and return true
// else return false, and the atomic value remains unchanged
@[inline]
pub fn (mut a AtomicVal[T]) compare_and_swap(expected T, new T) bool {
	$if T is bool {
		mut exp := u8(expected)
		return C.atomic_compare_exchange_strong_byte(voidptr(&a.val), &exp, u8(new))
	} $else $if T is u8 || T is i8 {
		mut exp := u8(expected)
		return C.atomic_compare_exchange_strong_byte(voidptr(&a.val), &exp, u8(new))
	} $else $if T is u16 || T is i16 {
		mut exp := u16(expected)
		return C.atomic_compare_exchange_strong_u16(voidptr(&a.val), &exp, u16(new))
	} $else $if T is u32 || T is i32 {
		mut exp := u32(expected)
		return C.atomic_compare_exchange_strong_u32(voidptr(&a.val), &exp, u32(new))
	} $else $if T is u64 || T is i64 {
		mut exp := u64(expected)
		return C.atomic_compare_exchange_strong_u64(voidptr(&a.val), &exp, u64(new))
	} $else $if T is int {
		if sizeof(int) == 4 {
			mut exp := u32(expected)
			return C.atomic_compare_exchange_strong_u32(voidptr(&a.val), &exp, u32(new))
		} else {
			mut exp := u64(expected)
			return C.atomic_compare_exchange_strong_u64(voidptr(&a.val), &exp, u64(new))
		}
	} $else $if T is isize || T is usize {
		if sizeof(isize) == 4 {
			mut exp := u32(expected)
			return C.atomic_compare_exchange_strong_u32(voidptr(&a.val), &exp, u32(new))
		} else {
			mut exp := u64(expected)
			return C.atomic_compare_exchange_strong_u64(voidptr(&a.val), &exp, u64(new))
		}
	} $else $if T is voidptr {
		// TODO: this should be $if sizeof(T) == 4
		$if x32 {
			mut exp := u32(expected)
			return C.atomic_compare_exchange_strong_u32(voidptr(&a.val), &exp, u32(new))
		} $else {
			mut exp := u64(expected)
			return C.atomic_compare_exchange_strong_u64(voidptr(&a.val), &exp, u64(new))
		}
	}
	panic('unreachable')
}
