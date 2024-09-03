struct Test {}

fn unmarshal[T]() ! {
	get_number[int]()!
}

fn get_number[T]() !T {
	return T(42)
}

fn test_generic_fn_with_nested_generic_fn_call() {
	unmarshal[Test]()!
	assert true
}
