fn test_conv_to_bool() {
	v := 0
	mut b := v != 0
	assert !b
	b = u64(&v) != 0
	assert b
}
