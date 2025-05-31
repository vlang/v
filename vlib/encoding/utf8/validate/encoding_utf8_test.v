import encoding.utf8.validate

fn test_validate_str() {
	assert validate.utf8_string('añçá') == true
	assert validate.utf8_string('\x61\xC3\xB1\xC3\xA7\xC3\xA1') == true

	assert validate.utf8_string('\x01') == true
	assert validate.utf8_string('\x7e') == true
	assert validate.utf8_string('\x7f') == true
	assert validate.utf8_string('\xc2\x80') == true
	assert validate.utf8_string('\xc2\x81') == true
	assert validate.utf8_string('\xc2\xbf') == true
	assert validate.utf8_string('\xc3\x80') == true
	assert validate.utf8_string('\xc3\x81') == true
	assert validate.utf8_string('\xc3\x88') == true
	assert validate.utf8_string('\xc3\x90') == true
	assert validate.utf8_string('\xc3\xa0') == true
	assert validate.utf8_string('\xc3\xb0') == true
	assert validate.utf8_string('\xc3\xb8') == true
	assert validate.utf8_string('\xc3\xbf') == true
	assert validate.utf8_string('\xc4\x80') == true
	assert validate.utf8_string('\xdf\xbf') == true
	assert validate.utf8_string('\xd0\x80') == true
	assert validate.utf8_string('\xe0\xa0\x80') == true
	assert validate.utf8_string('\xe0\xa0\x81') == true
	assert validate.utf8_string('\xe1\x80\x80') == true
	assert validate.utf8_string('\xed\x80\x80') == true
	assert validate.utf8_string('\xed\x9f\xbf') == true
	assert validate.utf8_string('\xee\x80\x80') == true
	assert validate.utf8_string('\xef\xbf\xbe') == true
	assert validate.utf8_string('\xef\xbf\xbf') == true
	assert validate.utf8_string('\xf0\x90\x80\x80') == true
	assert validate.utf8_string('\xf0\x90\x80\x81') == true
	assert validate.utf8_string('\xf1\x80\x80\x80') == true
	assert validate.utf8_string('\xf4\x8f\xbf\xbe') == true
	assert validate.utf8_string('\xf4\x8f\xbf\xbf') == true
	assert validate.utf8_string('\xef\xbf\xbd') == true
}

fn test_validate_invalid_str() {
	assert validate.utf8_string('\xC0\xC1') == false
	assert validate.utf8_string('\xF5\xFF') == false
	assert validate.utf8_string('\xE0\xEF') == false

	// xx
	assert validate.utf8_string('\x91\x80\x80\x80') == false

	// s1
	assert validate.utf8_string('\xC2\x7F\x80\x80') == false
	assert validate.utf8_string('\xC2\xC0\x80\x80') == false
	assert validate.utf8_string('\xDF\x7F\x80\x80') == false
	assert validate.utf8_string('\xDF\xC0\x80\x80') == false

	// s2
	assert validate.utf8_string('\xE0\x9F\xBF\x80') == false
	assert validate.utf8_string('\xE0\xA0\x7F\x80') == false
	assert validate.utf8_string('\xE0\xBF\xC0\x80') == false
	assert validate.utf8_string('\xE0\xC0\x80\x80') == false

	// s3
	assert validate.utf8_string('\xE1\x7F\xBF\x80') == false
	assert validate.utf8_string('\xE1\x80\x7F\x80') == false
	assert validate.utf8_string('\xE1\xBF\xC0\x80') == false
	assert validate.utf8_string('\xE1\xC0\x80\x80') == false

	// s4
	assert validate.utf8_string('\xED\x7F\xBF\x80') == false
	assert validate.utf8_string('\xED\x80\x7F\x80') == false
	assert validate.utf8_string('\xED\x9F\xC0\x80') == false
	assert validate.utf8_string('\xED\xA0\x80\x80') == false

	// s5
	assert validate.utf8_string('\xF0\x8F\xBF\xBF') == false
	assert validate.utf8_string('\xF0\x90\x7F\xBF') == false
	assert validate.utf8_string('\xF0\x90\x80\x7F') == false
	assert validate.utf8_string('\xF0\xBF\xBF\xC0') == false
	assert validate.utf8_string('\xF0\xBF\xC0\x80') == false
	assert validate.utf8_string('\xF0\xC0\x80\x80') == false

	// s6
	assert validate.utf8_string('\xF1\x7F\xBF\xBF') == false
	assert validate.utf8_string('\xF1\x80\x7F\xBF') == false
	assert validate.utf8_string('\xF1\x80\x80\x7F') == false
	assert validate.utf8_string('\xF1\xBF\xBF\xC0') == false
	assert validate.utf8_string('\xF1\xBF\xC0\x80') == false
	assert validate.utf8_string('\xF1\xC0\x80\x80') == false
}
