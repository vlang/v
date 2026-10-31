const byte = f64(8)

fn test_sizeof_byte_constant_uses_its_type() {
	assert sizeof(byte) == 8
}
