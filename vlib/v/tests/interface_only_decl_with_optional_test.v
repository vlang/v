interface Message {
	serialize() ?[]byte
}

fn test_interface_only_decl_with_optional() {
	println('test interface')
	assert true
}
