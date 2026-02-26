fn make_fixed() ![3]int {
	return [1, 10, 110].to_fixed_size()
}

fn test_array_literal_to_fixed_size_method() {
	fnums := [1, 10, 110].to_fixed_size()
	assert fnums == [1, 10, 110]!
	assert typeof(fnums).name == '[3]int'
}

fn test_array_literal_to_fixed_size_method_with_result_propagation() ! {
	fnums := make_fixed()!
	assert fnums == [1, 10, 110]!
}
