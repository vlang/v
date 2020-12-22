fn show_info(a string) string {
	return a
}

fn show_more_info(a string, b string) string {
	return a + b
}

fn test_interpolation_string_args() {
	assert '${show_info("abc")}' == 'abc'
	assert '${show_info('abc')}' == 'abc'

	assert '1_${show_info("aaa")} 2_${show_info("bbb")}' == '1_aaa 2_bbb'
	assert '1_${show_info('aaa')} 2_${show_info('bbb')}' == '1_aaa 2_bbb'

	assert '${"aaa"}' == 'aaa'
	assert '${'aaa'}' == 'aaa'

	assert '${"aaa" + "bbb"}' == 'aaabbb'
	assert '${'aaa' + 'bbb'}' == 'aaabbb'
	assert '${"aaa" + 'bbb'}' == 'aaabbb'
	assert '${'aaa' + "bbb"}' == 'aaabbb'

	assert '${show_more_info("aaa", "bbb")}' == 'aaabbb'
	assert '${show_more_info('aaa', 'bbb')}' == 'aaabbb'
	assert '${show_more_info("aaa", 'bbb')}' == 'aaabbb'
	assert '${show_more_info('aaa', "bbb")}' == 'aaabbb'

	assert '1_${show_more_info("aaa", "111")} 2_${show_more_info("bbb", "222")}' == '1_aaa111 2_bbb222'
	assert '1_${show_more_info('aaa', '111')} 2_${show_more_info('bbb', '222')}' == '1_aaa111 2_bbb222'
}
