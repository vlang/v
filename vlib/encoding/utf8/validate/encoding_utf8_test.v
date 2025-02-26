import encoding.utf8.validate

fn test_validate_str() {
	assert validate.utf8_string('añçá') == true
	assert validate.utf8_string('\x61\xC3\xB1\xC3\xA7\xC3\xA1') == true
	assert validate.utf8_string('\xC0\xC1') == false
	assert validate.utf8_string('\xF5\xFF') == false
	assert validate.utf8_string('\xE0\xEF') == false
}
