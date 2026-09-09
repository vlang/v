interface Value {}

fn empty_interface_bytes() []u8 {
	return [u8(1), 2, 3, 4]
}

fn test_array_value_is_not_spread_into_empty_interface_array() {
	mut values := []Value{}
	values << empty_interface_bytes()
	assert values.len == 1
	assert values[0] as []u8 == empty_interface_bytes()
}

fn test_array_variable_value_is_not_spread_into_empty_interface_array() {
	bytes := empty_interface_bytes()
	mut values := []Value{}
	values << bytes
	assert values.len == 1
	assert values[0] as []u8 == bytes
}

fn create_empty_interface_data() !(string, []Value) {
	name := 'hello'
	bytes := empty_interface_bytes()
	flag := true
	count := 7
	mut values := []Value{}
	values << name
	values << bytes
	values << flag
	values << count
	return 'ok', values
}

fn test_array_of_empty_interface_result_regression() {
	status, values := create_empty_interface_data() or { panic(err) }
	assert status == 'ok'
	assert values.len == 4
	rendered := '${values}'
	assert rendered.contains("Value('hello')")
	assert rendered.contains('Value([1, 2, 3, 4])')
	assert rendered.contains('Value(true)')
	assert rendered.contains('Value(7)')
	assert values[0] as string == 'hello'
	assert values[1] as []u8 == [u8(1), 2, 3, 4]
	assert values[2] as bool == true
	assert values[3] as int == 7
}
