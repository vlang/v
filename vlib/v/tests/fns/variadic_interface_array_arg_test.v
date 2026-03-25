interface VariadicValue {}

fn variadic_capture(values ...VariadicValue) []VariadicValue {
	return values
}

fn test_variadic_interface_array_variable_as_single_arg() {
	nested := [VariadicValue('brother')]
	got := variadic_capture(nested)
	assert got.len == 1

	inner := got[0] as []VariadicValue
	assert inner.len == 1
	assert (inner[0] as string) == 'brother'
}

fn test_variadic_interface_array_literal_as_single_arg() {
	got := variadic_capture([VariadicValue('x')])
	assert got.len == 1

	inner := got[0] as []VariadicValue
	assert inner.len == 1
	assert (inner[0] as string) == 'x'
}
