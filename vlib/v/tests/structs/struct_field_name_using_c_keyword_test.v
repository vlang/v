struct Keywords {
	typeof u8
}

fn test_struct_field_name_using_c_reserved() {
	key := Keywords{}
	println(key)
	assert true
}
