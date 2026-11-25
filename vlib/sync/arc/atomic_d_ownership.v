module arc

import sync.stdatomic

// Atomic is an owner of an atomic value shared with its clones.
pub struct Atomic[T] implements IClone, Drop {
mut:
	inner Arc[stdatomic.AtomicVal[T]]
}

// new_atomic creates an owner of an atomic value initialized to value.
pub fn new_atomic[T](value T) Atomic[T] {
	return Atomic[T]{
		inner: new(stdatomic.atomic_value(value))
	}
}

// clone creates another owner of the same atomic value.
pub fn (a &Atomic[T]) clone() Atomic[T] {
	return Atomic[T]{
		inner: a.inner.clone()
	}
}

// drop releases this reference to the atomic value.
pub fn (mut a Atomic[T]) drop() {
	a.inner.drop()
}

// load reads the current value atomically.
pub fn (a &Atomic[T]) load() T {
	// The reference stays inside the atomic operation.
	mut value := unsafe { a.inner.get() }
	return value.load()
}

// store replaces the current value atomically.
pub fn (a &Atomic[T]) store(next T) {
	// The reference stays inside the atomic operation.
	mut value := unsafe { a.inner.get() }
	value.store(next)
}

// add adds delta and returns the previous value.
pub fn (a &Atomic[T]) add(delta T) T {
	// The reference stays inside the atomic operation.
	mut value := unsafe { a.inner.get() }
	return value.add(delta)
}

// sub subtracts delta and returns the previous value.
pub fn (a &Atomic[T]) sub(delta T) T {
	// The reference stays inside the atomic operation.
	mut value := unsafe { a.inner.get() }
	return value.sub(delta)
}

// swap sets next and returns the previous value.
pub fn (a &Atomic[T]) swap(next T) T {
	// The reference stays inside the atomic operation.
	mut value := unsafe { a.inner.get() }
	return value.swap(next)
}

// compare_and_swap sets next when the current value equals expected.
pub fn (a &Atomic[T]) compare_and_swap(expected T, next T) bool {
	// The reference stays inside the atomic operation.
	mut value := unsafe { a.inner.get() }
	return value.compare_and_swap(expected, next)
}
