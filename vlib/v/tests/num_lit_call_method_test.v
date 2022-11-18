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
	assert x5 == '0.002'
	x6 := 5.0.str()
	assert x6 == '5.0'
	// x7 := 5..str()    Syntax `5.` is allowed, but do not call method on it (`5..str()` is parsed as a range). Use `5.0.str()` instead.
	x8 := 7.345e-7.str()
	assert x8 == '7.345e-07'
	x9 := 5.725e6.str()
	assert x9 == '5.725e+06'

	a := f32(12.3).str()
	assert a[0..4] == '12.3' // there is still trailing garbage
	assert f32(7.345e-7).str() == '7.345e-07'
	assert f32(5.725e6).str() == '5.725e+06'
}
