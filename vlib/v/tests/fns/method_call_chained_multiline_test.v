fn test_method_call_chained_multiline() {
	mut s := ' a b '
	// vfmt off
	s = s. trim_space().
		replace(' ', '').
		to_upper()
	// vfmt on
	assert s == 'AB'
}
