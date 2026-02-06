module lockfree

import sync.stdatomic

// Counter is a thread-safe atomic counter supporting multiple integer types.
// It provides atomic increment, decrement, and value retrieval operations.
@[noinit]
struct Counter[T] {
mut:
	value &stdatomic.AtomicVal[T]
}

// new_counter creates a new atomic counter with the specified initial value.
// It only supports integer types (8-bit to 64-bit integers).
pub fn new_counter[T](init T) &Counter[T] {
	// Compile-time type check: only integers are supported
	$if T !is $int {
		$compile_error('new_counter(): only integers are supported.')
	}
	return &Counter[T]{
		value: stdatomic.new_atomic[T](init)
	}
}

// increment_by atomically adds a delta value to the counter and returns
// the previous value before the operation (fetch-and-add).
@[inline]
pub fn (mut c Counter[T]) increment_by(delta T) T {
	return c.value.add(delta)
}

// increment atomically increments the counter by 1 and returns
// the previous value before the operation.
@[inline]
pub fn (mut c Counter[T]) increment() T {
	return c.increment_by(T(1))
}

// decrement_by atomically subtracts a delta value from the counter and returns
// the previous value before the operation (fetch-and-sub).
@[inline]
pub fn (mut c Counter[T]) decrement_by(delta T) T {
	return c.value.sub(delta)
}

// decrement atomically decrements the counter by 1 and returns
// the previous value before the operation.
@[inline]
pub fn (mut c Counter[T]) decrement() T {
	return c.decrement_by(T(1))
}

// get atomically retrieves the current value of the counter.
@[inline]
pub fn (mut c Counter[T]) get() T {
	return c.value.load()
}

// clear atomically resets the counter to zero.
@[inline]
pub fn (mut c Counter[T]) clear() {
	c.value.store(0)
}
