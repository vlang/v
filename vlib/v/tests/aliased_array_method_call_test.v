fn get_bytes_array() []byte {
	return [byte(97), 98, 99]
}

fn test_element_aliased_array_method_call() {
	assert get_bytes_array().bytestr() == 'abc'

	arr := [byte(97), 98, 99]
	assert arr.bytestr() == 'abc'
}
