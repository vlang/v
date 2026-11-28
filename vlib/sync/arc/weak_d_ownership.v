module arc

import sync.stdatomic

// Weak refers to an Arc allocation without keeping its value alive.
// Use `upgrade` to acquire a strong owner while the value is alive.
pub struct Weak[T] implements IClone, Drop {
	inner &ArcInner[T]
}

// downgrade creates a weak reference to the allocation shared by `a`.
pub fn (a &Arc[T]) downgrade() Weak[T] {
	assert !isnil(a.inner)
	_ = stdatomic.fetch_add_u64(&a.inner.weak, 1)
	return Weak[T]{
		inner: a.inner
	}
}

// clone creates another weak reference to the same allocation.
pub fn (w &Weak[T]) clone() Weak[T] {
	if !isnil(w.inner) {
		_ = stdatomic.fetch_add_u64(&w.inner.weak, 1)
	}
	return Weak[T]{
		inner: w.inner
	}
}

// upgrade acquires a strong reference if the value still has a strong owner.
pub fn (w &Weak[T]) upgrade() ?Arc[T] {
	if isnil(w.inner) {
		return none
	}
	for {
		strong := stdatomic.load_u64(&w.inner.strong)
		if strong == 0 {
			return none
		}
		// Only increment a nonzero count, even when the last Arc drops concurrently.
		if stdatomic.compare_and_swap_u64(&w.inner.strong, strong, strong + 1) {
			return Arc[T]{
				inner: w.inner
			}
		}
	}
	return none
}

// drop releases one weak reference and reclaims the allocation when it is the last reference.
pub fn (mut w Weak[T]) drop() {
	if isnil(w.inner) {
		return
	}
	// The handle retains the allocation, and release_weak only changes its atomic count.
	mut inner := unsafe { w.inner }
	inner.release_weak()
	w.inner = unsafe { nil }
}
