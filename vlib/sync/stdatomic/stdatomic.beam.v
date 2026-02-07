// BEAM backend stdatomic implementation
// Provides atomic operations using Erlang's :atomics module
//
// On BEAM, atomic operations are implemented via Erlang's atomics module.
// Since BEAM doesn't have direct memory access like C, we simulate atomics
// by treating pointers as values and using BEAM's built-in atomic primitives.
//
// Note: BEAM's concurrency model (actor-based) means most V code that uses
// atomics for thread-safety will work correctly even with these stubs,
// because BEAM processes don't share mutable memory.
module stdatomic

// Memory ordering - BEAM uses sequential consistency by default
pub enum MemoryOrder {
	relaxed
	consume
	acquire
	release
	acq_rel
	seq_cst
}

// add_u64 adds provided delta as an atomic operation
@[inline]
pub fn add_u64(ptr &u64, delta int) u64 {
	// On BEAM: atomics:add_get/3
	// Stub - actual implementation via codegen
	unsafe {
		mut p := ptr
		old := *p
		*p = u64(i64(old) + delta)
		return *p
	}
}

// sub_u64 subtracts provided delta as an atomic operation
@[inline]
pub fn sub_u64(ptr &u64, delta int) u64 {
	// On BEAM: atomics:sub_get/3
	unsafe {
		mut p := ptr
		old := *p
		*p = u64(i64(old) - delta)
		return *p
	}
}

// add_i64 adds provided delta as an atomic operation
@[inline]
pub fn add_i64(ptr &i64, delta int) i64 {
	// On BEAM: atomics:add_get/3
	unsafe {
		mut p := ptr
		old := *p
		*p = old + delta
		return *p
	}
}

// sub_i64 subtracts provided delta as an atomic operation
@[inline]
pub fn sub_i64(ptr &i64, delta int) i64 {
	// On BEAM: atomics:sub_get/3
	unsafe {
		mut p := ptr
		old := *p
		*p = old - delta
		return *p
	}
}

// store_u64 atomically sets a value
@[inline]
pub fn store_u64(ptr &u64, val u64) {
	// On BEAM: atomics:put/3
	unsafe {
		mut p := ptr
		*p = val
	}
}

// load_u64 atomically gets a value
@[inline]
pub fn load_u64(ptr &u64) u64 {
	// On BEAM: atomics:get/2
	return *ptr
}

// store_i64 atomically sets a value
@[inline]
pub fn store_i64(ptr &i64, val i64) {
	// On BEAM: atomics:put/3
	unsafe {
		mut p := ptr
		*p = val
	}
}

// load_i64 atomically gets a value
@[inline]
pub fn load_i64(ptr &i64) i64 {
	// On BEAM: atomics:get/2
	return *ptr
}

// exchange_u64 atomically sets a value and returns the old value
@[inline]
pub fn exchange_u64(ptr &u64, val u64) u64 {
	// On BEAM: atomics:exchange/3
	unsafe {
		mut p := ptr
		old := *p
		*p = val
		return old
	}
}

// exchange_i64 atomically sets a value and returns the old value
@[inline]
pub fn exchange_i64(ptr &i64, val i64) i64 {
	// On BEAM: atomics:exchange/3
	unsafe {
		mut p := ptr
		old := *p
		*p = val
		return old
	}
}

// compare_exchange_strong_u64 performs compare-and-swap
// Returns true if exchange was performed
@[inline]
pub fn compare_exchange_strong_u64(ptr &u64, expected &u64, desired u64) bool {
	// On BEAM: atomics:compare_exchange/4
	unsafe {
		mut p := ptr
		if *p == *expected {
			*p = desired
			return true
		}
		mut e := expected
		*e = *p
		return false
	}
}

// compare_exchange_strong_i64 performs compare-and-swap
@[inline]
pub fn compare_exchange_strong_i64(ptr &i64, expected &i64, desired i64) bool {
	// On BEAM: atomics:compare_exchange/4
	unsafe {
		mut p := ptr
		if *p == *expected {
			*p = desired
			return true
		}
		mut e := expected
		*e = *p
		return false
	}
}

// compare_exchange_weak_u64 performs compare-and-swap (may fail spuriously)
// On BEAM, this is the same as strong version
@[inline]
pub fn compare_exchange_weak_u64(ptr &u64, expected &u64, desired u64) bool {
	return compare_exchange_strong_u64(ptr, expected, desired)
}

// compare_exchange_weak_i64 performs compare-and-swap (may fail spuriously)
@[inline]
pub fn compare_exchange_weak_i64(ptr &i64, expected &i64, desired i64) bool {
	return compare_exchange_strong_i64(ptr, expected, desired)
}

// --- 32-bit variants ---

// add_u32 adds provided delta as an atomic operation
@[inline]
pub fn add_u32(ptr &u32, delta int) u32 {
	unsafe {
		mut p := ptr
		old := *p
		*p = u32(i32(old) + delta)
		return *p
	}
}

// sub_u32 subtracts provided delta as an atomic operation
@[inline]
pub fn sub_u32(ptr &u32, delta int) u32 {
	unsafe {
		mut p := ptr
		old := *p
		*p = u32(i32(old) - delta)
		return *p
	}
}

// store_u32 atomically sets a value
@[inline]
pub fn store_u32(ptr &u32, val u32) {
	unsafe {
		mut p := ptr
		*p = val
	}
}

// load_u32 atomically gets a value
@[inline]
pub fn load_u32(ptr &u32) u32 {
	return *ptr
}

// exchange_u32 atomically sets a value and returns the old value
@[inline]
pub fn exchange_u32(ptr &u32, val u32) u32 {
	unsafe {
		mut p := ptr
		old := *p
		*p = val
		return old
	}
}

// compare_exchange_strong_u32 performs compare-and-swap
@[inline]
pub fn compare_exchange_strong_u32(ptr &u32, expected &u32, desired u32) bool {
	unsafe {
		mut p := ptr
		if *p == *expected {
			*p = desired
			return true
		}
		mut e := expected
		*e = *p
		return false
	}
}

// --- AtomicVal generic wrapper ---

@[heap]
pub struct AtomicVal[T] {
pub mut:
	val T
}

// new_atomic creates a new atomic value of T type
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
	return a.val
}

// store updates the atomic value with val
@[inline]
pub fn (mut a AtomicVal[T]) store(val T) {
	unsafe {
		a.val = val
	}
}

// add adds the atomic value with delta and returns the previous value
@[inline]
pub fn (mut a AtomicVal[T]) add(delta T) T {
	$if T is bool {
		panic('atomic: add() not supported for bool type')
	} $else $if T is voidptr {
		panic('atomic: add() not supported for voidptr type')
	}
	old := a.val
	unsafe {
		$if T is $int {
			a.val = T(int(a.val) + int(delta))
		}
	}
	return old
}

// sub subtracts the atomic value with delta and returns the previous value
@[inline]
pub fn (mut a AtomicVal[T]) sub(delta T) T {
	$if T is bool {
		panic('atomic: sub() not supported for bool type')
	} $else $if T is voidptr {
		panic('atomic: sub() not supported for voidptr type')
	}
	old := a.val
	unsafe {
		$if T is $int {
			a.val = T(int(a.val) - int(delta))
		}
	}
	return old
}

// swap sets the new value and returns the previous value
@[inline]
pub fn (mut a AtomicVal[T]) swap(new T) T {
	old := a.val
	unsafe {
		a.val = new
	}
	return old
}

// compare_and_swap executes the compare-and-swap(CAS) operation
// if atomic value == expected, then it will be set to new, and return true
// else return false, and the atomic value remains unchanged
@[inline]
pub fn (mut a AtomicVal[T]) compare_and_swap(expected T, new T) bool {
	if a.val == expected {
		unsafe {
			a.val = new
		}
		return true
	}
	return false
}

// --- Thread fence ---

// thread_fence provides memory fence/barrier
@[inline]
pub fn thread_fence(order MemoryOrder) {
	// On BEAM: No-op - BEAM has sequential consistency
	// BEAM's process model provides stronger guarantees
}

// cpu_relax is a hint to the processor for spin-wait loops
@[inline]
pub fn cpu_relax() {
	// On BEAM: No-op - Erlang scheduler handles this
}
