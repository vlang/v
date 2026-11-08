module strings

fn test_to_valid_utf8() {
	assert to_valid_utf8('abc', '') == 'abc'
}
