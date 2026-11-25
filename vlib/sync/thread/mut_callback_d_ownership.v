module thread

interface MutCallable[A, R] {
mut:
	call(arg A) R
}

// MutCallback owns a transferable callback that runs with exclusive access to its state.
@[noinit]
pub struct MutCallback[A, R] implements Drop {
mut:
	value MutCallable[A, R]
}

// new_mut_callback moves a callable value into a transferable callback.
pub fn new_mut_callback[A, R Transferable, T Transferable](value T) MutCallback[A, R] {
	return MutCallback[A, R]{
		value: MutCallable[A, R](value)
	}
}

// call invokes the callback with exclusive access to its state.
pub fn (mut callback MutCallback[A, R]) call(arg A) R {
	return callback.value.call(arg)
}

// drop releases the callback and its state.
pub fn (mut callback MutCallback[A, R]) drop() {
	drop_owned(callback.value)
}
