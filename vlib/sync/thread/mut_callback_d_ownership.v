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

// new_mut_callback moves a value with a public, mutable borrowing call method into a callback.
pub fn new_mut_callback[A, R Transferable, T Transferable](value T) MutCallback[A, R] {
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
