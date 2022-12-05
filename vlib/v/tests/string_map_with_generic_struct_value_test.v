import datatypes

fn test_string_map_with_generic_struct_value() {
	p := map[string]datatypes.Stack[int]{}
	println(p)
	assert '${p}' == '{}'
}
