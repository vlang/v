fn test_replace_once() {
	assert 'abc'.replace_once('', 'X') == 'Xabc'
	assert ''.replace_once('', 'X') == 'X'
	assert 'abc'.replace_once('', '') == 'abc'
	assert ''.replace_once('', '') == ''
	assert 'abcabc'.replace_once('abc', 'X') == 'Xabc'
	assert 'abc'.replace_once('missing', 'X') == 'abc'
	assert 'é🙂'.replace_once('', '前') == '前é🙂'
}
