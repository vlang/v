interface Message {
	serialize() ?[]u8
}

fn test_interface_only_decl_with_option() {
	println('test interface')
	assert true
}
