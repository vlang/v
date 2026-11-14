module bytes

fn test_to_valid_utf8() {
	assert to_valid_utf8('abc'.bytes(), ''.bytes()) == 'abc'.bytes()
}
