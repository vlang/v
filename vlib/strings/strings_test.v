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

const test_rune_and_byte = [
	'xxx[ok1]xxx',
	'xxx[[ok2]okok]',
	'xxx[ok3[[[ok]okok]]]',
	'yyy[ok4]',
	'[]',
	']',
	'[',
	'yyy[ok5][]zzz',
	'yyy[xxx',
	'xxx[xxx
	xxx]',
]

const test_strings = [
	'xxx/*ok1*/xxx',
	'xxx/*/*ok2*/okok*/',
	'xxx/*ok3/*/*/*ok*/okok*/*/*/',
	'yyy/*ok4*/',
	'/**/',
	'*/',
	'/*',
	'yyy/*ok5*//**/zzz',
	'yyy/*xxx',
	'xxx/*xxx
	xxx*/xxx',
]

const expected_rune_and_byte_outputs = [
	'ok1',
	'[ok2]okok',
	'ok3[[[ok]okok]]',
	'ok4',
	'',
	'',
	'',
	'ok5',
	'',
	'xxx
	xxx',
]

const expected_string_outputs = [
	'ok1',
	'/*ok2*/okok',
	'ok3/*/*/*ok*/okok*/*/',
	'ok4',
	'',
	'',
	'',
	'ok5',
	'',
	'xxx
	xxx',
]

fn test_find_between_pair_family() {
	assert strings.find_between_pair_rune('xx♡ok❦yy', `♡`, `❦`) == 'ok'
	assert strings.find_between_pair_byte('xx{ok}yy', `{`, `}`) == 'ok'
	assert strings.find_between_pair_string('xx/*ok*/yy', '/*', '*/') == 'ok'
	assert strings.find_between_pair_byte('xx{ok}yy', `{`, `}`) == 'ok'
	assert strings.find_between_pair_string('xxxxokyyyy', 'xxx', 'yyy') == 'xok'

	for i, tstr in test_rune_and_byte {
		e1 := strings.find_between_pair_rune(tstr, `[`, `]`)
		e2 := expected_rune_and_byte_outputs[i]
		assert '$e1' == '$e2'
	}

	for i, tstr in test_rune_and_byte {
		e1 := strings.find_between_pair_byte(tstr, `[`, `]`)
		e2 := expected_rune_and_byte_outputs[i]
		assert '$e1' == '$e2'
	}

	for i, tstr in test_strings {
		e1 := strings.find_between_pair_string(tstr, '/*', '*/')
		e2 := expected_string_outputs[i]
		assert '$e1' == '$e2'
	}
}
