fn test_string_interpolation_reference_default_fmt() {
	s := 'Hello'
	sp := &s
	assert '${sp}' == ptr_str(sp)
}
