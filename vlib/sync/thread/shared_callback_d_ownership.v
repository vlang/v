module thread

import sync.arc

interface Callable[A, R] {
	call(arg A) R
}

// SharedCallback owns a callback whose state can be used from multiple threads.
@[noinit]
pub struct SharedCallback[A, R] implements IClone, Drop {
mut:
	inner arc.Arc[Callable[A, R]]
}

// new_shared_callback moves shareable state with a public borrowing call method into a callback.
pub fn new_shared_callback[A, R Transferable, T Shareable](value T) SharedCallback[A, R] {
	$if T !is Callable[A, R] {
		$compile_error('the callback state must implement call with the requested signature')
	}
	$for method in T.methods {
		$if method.name == 'call' {
			invoke := T.$method
			$if invoke is fn(&T, A) R {
				_ = invoke
			} $else {
				$compile_error('the shared callback call method requires a borrowed receiver')
			}
		}
	}
	return SharedCallback[A, R]{
		inner: arc.new(Callable[A, R](value))
	}
}

// call invokes the callback with shared access to its state.
pub fn (callback &SharedCallback[A, R]) call(arg A) R {
	return callback.inner.get().call(arg)
}

// clone returns another owner of the shared callback.
pub fn (callback &SharedCallback[A, R]) clone() SharedCallback[A, R] {
	return SharedCallback[A, R]{
		inner: callback.inner.clone()
	}
}

// drop releases one owner of the shared callback.
pub fn (mut callback SharedCallback[A, R]) drop() {
	callback.inner.drop()
}
