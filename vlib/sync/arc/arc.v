// Module arc provides atomically reference-counted shared ownership.
// Arc keeps a value alive. Weak keeps its allocation available for an upgrade.
module arc

import sync.stdatomic

@[heap]
struct ArcInner[T] {
mut:
	strong u64
	// Includes one reference held by the strong owners until `value` is destroyed.
	weak  u64
	value T
}

// new moves `value` into a new atomically reference-counted allocation.
pub fn new[T](value T) Arc[T] {
	return Arc[T]{
		inner: &ArcInner[T]{
			strong: 1
			weak:   1
			value:  value
		}
	}
}

// clone creates another owner of the same allocation.
pub fn (a &Arc[T]) clone() Arc[T] {
	assert !isnil(a.inner)
	_ = stdatomic.fetch_add_u64(&a.inner.strong, 1)
	return Arc[T]{
		inner: a.inner
	}
}

// strong_count returns the number of strong references to this allocation.
// The count can change immediately when other threads clone or drop the Arc.
pub fn (a &Arc[T]) strong_count() u64 {
	assert !isnil(a.inner)
	return stdatomic.load_u64(&a.inner.strong)
}

// ptr_eq reports whether two Arc values point to the same allocation.
pub fn ptr_eq[T](a &Arc[T], b &Arc[T]) bool {
	return voidptr(a.inner) == voidptr(b.inner)
}

// drop releases one strong reference and destroys `T` after the last release.
pub fn (mut a Arc[T]) drop() {
	if isnil(a.inner) {
		return
	}
	if stdatomic.fetch_sub_u64(&a.inner.strong, 1) == 1 {
		// A zero strong count prevents upgrades while the last owner destroys the payload.
		mut inner := unsafe { a.inner }
		drop_owned(inner.value)
		inner.release_weak()
	}
	a.inner = unsafe { nil }
}

fn (mut inner ArcInner[T]) release_weak() {
	if stdatomic.fetch_sub_u64(&inner.weak, 1) == 1 {
		// The last weak reference can reclaim the allocation after payload destruction.
		unsafe { free(inner) }
	}
}
