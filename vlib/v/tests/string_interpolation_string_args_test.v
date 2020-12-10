fn show_info(a string) string {
	return a
}

fn test_interpolation_string_args() {
	assert '${show_info("abc")}' == 'abc'
	assert '${show_info('abc')}' == 'abc'

	assert '1_${show_info("aaa")} 2_${show_info("bbb")}' == '1_aaa 2_bbb'
	assert '1_${show_info('aaa')} 2_${show_info('bbb')}' == '1_aaa 2_bbb'

	assert '${"aaa"}' == 'aaa'
	assert '${'aaa'}' == 'aaa'
}
