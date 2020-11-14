import strings

fn test_repeat() {
	assert strings.repeat(`x`, 10) == 'xxxxxxxxxx'
	assert strings.repeat(`a`, 1) == 'a'
	assert strings.repeat(`a`, 0) == ''
}
