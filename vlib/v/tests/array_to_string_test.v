fn test_array_to_string_conversion() {
	expected := '["1", "2", "3", "4"]'
	arr := ['1', '2', '3', '4']
	assert arr.str() == expected
}

fn test_interpolation_array_to_string() {
	a := ['1', '2', '3']
	assert '$a' == '["1", "2", "3"]'
	b := ['a', 'b']
	assert '$b' == '["a", "b"]'
}
