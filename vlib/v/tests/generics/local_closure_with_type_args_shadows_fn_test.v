// The program has a function `callback` that returns `?bool`. In `wrap`, the local
// closure `callback`, called with explicit type arguments, shadows it.
fn callback(x int) ?bool {
	return x > 0
}

fn identity[T](value T) T {
	return value
}

fn wrap[T](value T) ?T {
	callback := fn [value] [T]() T {
		return identity[T](value)
	}
	return callback[T]()
}

fn test_local_closure_called_with_type_args_shadows_a_function_of_its_name() {
	assert callback(1) or { false }
	assert wrap[int](7) or { 0 } == 7
}
