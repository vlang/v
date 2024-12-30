fn function_that_receives_an_array_of_generic_type[T](array []T) {
	return
}

fn function_that_returns_an_array_of_generic_type[T]() []T {
	return []
}

fn func[T]() {
	res := function_that_returns_an_array_of_generic_type[T]()
	function_that_receives_an_array_of_generic_type(res)
	function_that_receives_an_array_of_generic_type[T](res)
	return
}

fn test_main() {
	func[string]()
}
