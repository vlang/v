fn identity[T](value T) T {
	return value
}

fn wrap[T](value T) ?T {
	cb := fn [value] [T]() T {
		return identity[T](value)
	}
	return cb[T]()
}

fn twice[T](value T) []T {
	pair := fn [T](x T) []T {
		return [x, x]
	}
	return pair[T](value)
}

// Each instance calls its local closure, which its explicit type arguments only
// typed: a second instance used to keep `(cb[T])()` in the generated C.
fn test_closures_with_type_args_in_several_instances() {
	assert wrap[int](7) or { 0 } == 7
	assert wrap[string]('x') or { '' } == 'x'
	assert twice[int](3) == [3, 3]
	assert twice[string]('a') == ['a', 'a']
}
