import rand

interface VariadicValue {}

fn variadic_capture(values ...VariadicValue) []VariadicValue {
	return values
}

fn variadic_bytes() []u8 {
	return [u8(1), 2, 3]
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

fn test_variadic_interface_array_variable_with_additional_args() {
	bytes := variadic_bytes()
	got := variadic_capture(bytes, 'tail')
	assert got.len == 2
	assert got[0] as []u8 == bytes
	assert got[1] as string == 'tail'
}

fn test_variadic_interface_array_result_with_additional_args() ! {
	for _ in 0 .. 3 {
		bytes := rand.bytes(16)!
		got := variadic_capture(bytes, 'tail')
		assert got.len == 2
		assert got[0] as []u8 == bytes
		assert got[1] as string == 'tail'
	}
}
