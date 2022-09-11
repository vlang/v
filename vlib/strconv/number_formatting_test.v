import strconv

fn test_int_formatting() {
	assert '-10,000,000,000' == strconv.int_format(-10000000000, ',')
}

fn test_uint_formatting() {
	assert '100,000' == strconv.uint_format(100000, ',')
}

fn test_f32_formatting() {
	assert '10,000.45' == strconv.f32_format(10000.45, ',', '.')
}

fn test_f64_formatting() {
	assert '10,000.45676576789' == strconv.f64_format(10000.45676576789, ',', '.')
}
