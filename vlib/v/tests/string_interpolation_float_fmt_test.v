fn test_string_interpolation_float_fmt() {
	mut a := 76.295
	eprintln('${a:8.2}')
	assert '${a:8.2}' == '   76.30'
	eprintln('${a:8.2f}')
	assert '${a:8.2f}' == '   76.30'

	a = 76.296
	eprintln('${a:8.2}')
	assert '${a:8.2}' == '   76.30'
	eprintln('${a:8.2f}')
	assert '${a:8.2f}' == '   76.30'
}
