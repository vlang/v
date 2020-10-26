import encoding.utf8

fn test_validate_str() {
	assert utf8.validate_str('añçá') == true
	assert utf8.validate_str('\x61\xC3\xB1\xC3\xA7\xC3\xA1') == true
	assert utf8.validate_str('\xC0\xC1') == false
	assert utf8.validate_str('\xF5\xFF') == false
	assert utf8.validate_str('\xE0\xEF') == false
}
