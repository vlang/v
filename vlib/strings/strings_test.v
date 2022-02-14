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

const test_runes = [
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

const expected_rune_outputs = [
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

fn test_find_between_pair() {
	assert strings.find_between_pair('xx♡ok❦yy', `♡`, `❦`) == 'ok'
	assert strings.find_between_pair('xx{ok}yy', byte(`{`), byte(`}`)) == 'ok'
	assert strings.find_between_pair('xx/*ok*/yy', '/*', '*/') == 'ok'
	assert strings.find_between_pair('xx{ok}yy', `{`, `}`) == 'ok'
	assert strings.find_between_pair('xx{ok}yy', byte(`{`), byte(`}`)) == 'ok'

	for i, tstr in test_runes {
		e1 := strings.find_between_pair(tstr, `[`, `]`)
		e2 := expected_rune_outputs[i]
		assert '$e1' == '$e2'
	}

	for i, tstr in test_strings {
		e1 := strings.find_between_pair(tstr, '/*', '*/')
		e2 := expected_string_outputs[i]
		assert '$e1' == '$e2'
	}
}
