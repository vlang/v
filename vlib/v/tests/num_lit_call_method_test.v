fn test_int_lit_call_method() {
	x1 := 1234.str()
	assert x1 == '1234'
	x2 := -0xffff.str()
	assert x2 == '-65535'
	x3 := 0b1001001.str()
	assert x3 == '73'
	x4 := 0o653262.str()
	assert x4 == '218802'
	x5 := 0.str()
	assert x5 == '0'
}

fn test_float_lit_call_method() {
	x1 := -123.66.str()
	assert x1 == '-123.66'
	x2 := 12.5e-2.str()
	assert x2 == '0.125'
	x3 := .789.str()
	assert x3 == '0.789'
	x4 := .003e2.str()
	assert x4 == '0.3'
	x5 := 2.e-3.str()
	assert x5 == '2.e-03'
	x6 := 5.0.str()
	assert x6 == '5.'
	// x7 := 5..str()    Syntax `5.` is allowed, but do not call method on it (`5..str()` is parsed as a range). Use `5.0.str()` instead.
}
