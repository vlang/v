module cell

// RefCell is a container with runtime-checked borrowing: many shared borrows
// may coexist, but an exclusive borrow excludes all others, inspired by
// Rust's `std::cell::RefCell`. Unlike `shared` variables it is single-threaded
// and needs no locks; unlike plain references the checks happen at runtime.
// Access goes through callbacks, since V has no guard lifetimes.
@[heap]
pub struct RefCell[T] {
mut:
	value   T
	borrows int // >0: shared borrows held; -1: exclusively borrowed
}

// new_ref_cell creates a RefCell holding value, initially unborrowed.
pub fn new_ref_cell[T](value T) &RefCell[T] {
	return &RefCell[T]{
		value: value
	}
}

// with runs f on the value under a shared borrow and returns its result.
// It panics if the cell is exclusively borrowed.
pub fn (mut c RefCell[T]) with(f fn (v T) T) T {
	return c.try_with(f) or { panic('cell: already mutably borrowed') }
}

// try_with is with without the panic: it returns none if the cell is
// exclusively borrowed.
pub fn (mut c RefCell[T]) try_with(f fn (v T) T) ?T {
	if c.borrows < 0 {
		return none
	}
	c.borrows++
	defer {
		c.borrows--
	}
	return f(c.value)
}

// with_mut runs f on the value under an exclusive borrow and stores the
// result back. It panics if the cell is borrowed in any way.
pub fn (mut c RefCell[T]) with_mut(f fn (v T) T) {
	if !c.try_with_mut(f) {
		panic('cell: already borrowed')
	}
}

// try_with_mut is with_mut without the panic: it returns false (leaving the
// value untouched) if the cell is borrowed in any way.
pub fn (mut c RefCell[T]) try_with_mut(f fn (v T) T) bool {
	if c.borrows != 0 {
		return false
	}
	c.borrows = -1
	defer {
		c.borrows = 0
	}
	c.value = f(c.value)
	return true
}

// try_borrow returns the value using ordinary V value semantics, or none if
// the cell is exclusively borrowed. This access is not tracked afterwards;
// references contained in T are not deep-cloned. Update through with_mut.
pub fn (c &RefCell[T]) try_borrow() ?T {
	if c.borrows < 0 {
		return none
	}
	return c.value
}
