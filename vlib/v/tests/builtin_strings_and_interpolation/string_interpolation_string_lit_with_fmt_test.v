fn test_string_interpolation_string_lit_with_fmt() {
	println('${'hello':-12s}')
	assert '${'hello':-12s}' == 'hello       '
	println('${'hello':12s}')
	assert '${'hello':12s}' == '       hello'
}
