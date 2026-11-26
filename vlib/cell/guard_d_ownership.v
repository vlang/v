module cell

// Ref keeps a shared runtime borrow of a RefCell until the guard is dropped.
@[noinit]
pub struct Ref[^a, T] implements Drop {
	cell &^a RefCell[T]
}

// RefMut keeps an exclusive runtime borrow of a RefCell until the guard is dropped.
@[noinit]
pub struct RefMut[^a, T] implements Drop {
	cell &^a RefCell[T]
}

// borrow acquires a shared guard. It panics while an exclusive guard is held.
pub fn (owner &^a RefCell[T]) borrow[^a]() Ref[^a, T] {
	return owner.try_borrow_ref() or { panic('cell: already mutably borrowed') }
}

// try_borrow_ref acquires a shared guard or returns none on an exclusive borrow.
pub fn (owner &^a RefCell[T]) try_borrow_ref[^a]() ?Ref[^a, T] {
	if owner.borrows < 0 {
		return none
	}
	// RefCell is single-threaded and its borrow counter supports interior mutation.
	unsafe {
		mut state := owner
		state.borrows++
	}
	return Ref[^a, T]{ cell: owner }
}

// borrow_mut acquires an exclusive guard. It panics while any borrow is held.
pub fn (owner &^a RefCell[T]) borrow_mut[^a]() RefMut[^a, T] {
	return owner.try_borrow_mut() or { panic('cell: already borrowed') }
}

// try_borrow_mut acquires an exclusive guard or returns none on a conflict.
pub fn (owner &^a RefCell[T]) try_borrow_mut[^a]() ?RefMut[^a, T] {
	if owner.borrows != 0 {
		return none
	}
	// A zero count grants this guard exclusive access to the value.
	unsafe {
		mut state := owner
		state.borrows = -1
	}
	return RefMut[^a, T]{ cell: owner }
}

// get borrows the value for the lifetime of the shared guard reference.
pub fn (guard &^b Ref[^a, T]) get[^a, ^b]() &^b T {
	return &guard.cell.value
}

// get borrows the value for the lifetime of the exclusive guard reference.
pub fn (guard &^b RefMut[^a, T]) get[^a, ^b]() &^b T {
	return &guard.cell.value
}

// get_mut grants mutable access to the value through the exclusive guard.
pub fn (mut guard ^b RefMut[^a, T]) get_mut[^a, ^b]() &^b T {
	return &guard.cell.value
}

fn (mut guard Ref[^a, T]) drop[^a]() {
	// The guard releases its own share of the runtime borrow.
	unsafe {
		mut state := guard.cell
		state.borrows--
	}
}

fn (mut guard RefMut[^a, T]) drop[^a]() {
	// Dropping the exclusive guard makes the value available again.
	unsafe {
		mut state := guard.cell
		state.borrows = 0
	}
}
