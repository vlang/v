import strings

fn test_repeat() {
	assert strings.repeat(`x`, 10) == 'xxxxxxxxxx'
	assert strings.repeat(`a`, 1) == 'a'
	assert strings.repeat(`a`, 0) == ''
}

fn test_repeat_string() {
	assert strings.repeat_string('abc', 3) == 'abcabcabc'
	assert strings.repeat_string('abc', 1) == 'abc'
	assert strings.repeat_string('abc', 0) == ''
	assert strings.repeat_string('', 200) == ''
}
