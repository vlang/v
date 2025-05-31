fn get_bytes_array() []u8 {
	return [u8(97), 98, 99]
}

fn test_element_aliased_array_method_call() {
	assert get_bytes_array().bytestr() == 'abc'

	arr := [u8(97), 98, 99]
	assert arr.bytestr() == 'abc'
}
