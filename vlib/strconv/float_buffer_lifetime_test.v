import strconv

fn test_float_strings_keep_independent_storage() {
	values := [f64(-0.125), 0.0, 1.5, 123456.75]
	expected := ['-0.125', '0.0', '1.5', '123456.75']
	mut saved := []string{}
	for value in values {
		saved << strconv.f64_to_str_l(value)
	}
	for i in 0 .. 1024 {
		assert strconv.f64_to_str_l(f64(i) + 0.5).ends_with('.5')
	}
	gc_collect()
	assert saved == expected
}

fn test_scientific_float_buffers_preserve_large_padding() {
	short := strconv.f64_to_str(1.25, 18)
	wide := strconv.f64_to_str_pad(1.25, 128)
	single := strconv.f32_to_str(f32(-1.25), 8)
	integer := strconv.f32_to_str(f32(7), 0)
	assert short == '1.25e+00'
	assert wide == '1.25' + '0'.repeat(126) + 'e+00'
	assert single == '-1.25e+00'
	assert integer == '7e+00'
	assert strconv.fxx_to_str_l_parse('1.25e+128').len == 131
	assert strconv.fxx_to_str_l_parse_with_dot('1.25e-128').len == 132
	gc_collect()
	assert short == '1.25e+00'
	assert wide.ends_with('e+00')
	assert single == '-1.25e+00'
	assert integer == '7e+00'
}

fn test_f32_rounding_carries_into_the_exponent() {
	assert strconv.f32_to_str(f32(9.75), 0) == '1e+01'
	assert strconv.f32_to_str(f32(-9.75), 0) == '-1e+01'
	assert strconv.f32_to_str(f32(99.75), 1) == '1.0e+02'
	assert strconv.f32_to_str_pad(f32(1.25), 128) == '1.25' + '0'.repeat(126) + 'e+00'
}
