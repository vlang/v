import encoding.utf8 { validate_str }

fn test_validate_str() {
	assert validate_str('añçá') == true
	assert validate_str('\x61\xC3\xB1\xC3\xA7\xC3\xA1') == true
	assert validate_str('\xC0\xC1') == false
	assert validate_str('\xF5\xFF') == false
	assert validate_str('\xE0\xEF') == false
}
