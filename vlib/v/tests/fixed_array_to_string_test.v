fn test_fixed_array_to_string_conversion() {
	a := ['1', '2', '3', '4']!!
	assert a.str() == '["1", "2", "3", "4"]'
	b := [1, 2, 3, 4]!!
	assert b.str() == '[1, 2, 3, 4]'
}

fn test_interpolation_fixed_array_to_string() {
	a := ['1', '2', '3']!!
	assert '$a' == '["1", "2", "3"]'
	b := ['a', 'b']!!
	assert '$b' == '["a", "b"]'
	c := [1, 2, 3]!!
	assert '$c' == '[1, 2, 3]'
	d := [1.1, 2.2, 3.3]!!
	assert '$d' == '[1.1, 2.2, 3.3]'
}
