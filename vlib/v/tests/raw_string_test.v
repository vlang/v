fn test_raw_string_backslash() {
	assert r'\' == r'\'
}

fn test_raw_string_not_escaped_by_transformer() {
	assert r'a\nb' + r'a\nb' == r'a\nba\nb'
	assert 'a\nb' + r'a\nb' == 'a\nba\\nb'
}
