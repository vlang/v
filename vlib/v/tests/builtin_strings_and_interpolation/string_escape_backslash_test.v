fn test_escaped_backslash_after_string_interpolation() {
	test := 'test'
	a := "\\\"${test}\\\""
	assert a == '\\"test\\"'
}
