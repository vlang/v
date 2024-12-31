fn function_that_receives_an_array_of_generic_type[T](array []T) {
	assert array.len == 0
}

fn function_that_returns_an_array_of_generic_type[T]() []T {
	return []
}

fn func[T]() {
	res := function_that_returns_an_array_of_generic_type[T]()
	function_that_receives_an_array_of_generic_type(res)
	assert res.len == 0
	function_that_receives_an_array_of_generic_type[T](res)
}

fn test_main() {
	func[string]()
	func[int]()
	func[f64]()
}
