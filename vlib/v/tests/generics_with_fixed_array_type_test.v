fn show_element<T>(arr T) string {
	return '${arr[1]}'
}

fn test_generic_with_fixed_array_type() {
	a := [1, 2, 3]!
	ret := show_element(a)
	println(ret)
	assert ret == '2'
}
