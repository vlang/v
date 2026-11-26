module thread

// ScopedMutCallback owns a callback that can retain caller borrows for a checked scoped invocation.
@[noinit]
pub struct ScopedMutCallback[^scope, A, R] implements Drop {
mut:
	value MutCallable[A, R]
}

// new_scoped_mut_callback moves mutable callback state into a scoped callback.
pub fn new_scoped_mut_callback[^scope, A, R Transferable, T](value T) ScopedMutCallback[^scope, A, R] {
	$if autofree {
		$compile_error('scoped mutable callbacks require ownership moves, which autofree does not track')
	}
	$if T !is MutCallable[A, R] {
		$compile_error('the callback state must implement call with the requested signature')
	}
	$for method in T.methods {
		$if method.name == 'call' {
			invoke := T.$method
			$if invoke is fn(mut T, A) R {
				_ = invoke
			} $else {
				$compile_error('the mutable callback call method requires a mutable borrowed receiver')
			}
		}
	}
	return ScopedMutCallback[^scope, A, R]{
		value: MutCallable[A, R](value)
	}
}

// call invokes the callback with exclusive access to its state while keeping the box.
pub fn (mut callback ScopedMutCallback[^scope, A, R]) call(arg A) R {
	return callback.value.call(arg)
}

// invoke_scoped_mut_callback takes ownership of a callback, calls it once, and releases its state.
pub fn invoke_scoped_mut_callback[^scope, A, R Transferable](callback ScopedMutCallback[^scope, A, R], arg A) R {
	mut owned := callback
	defer {
		drop_owned(owned)
	}
	return owned.call(arg)
}

// drop releases the callback and its state.
pub fn (mut callback ScopedMutCallback[^scope, A, R]) drop() {
	drop_owned(callback.value)
}
