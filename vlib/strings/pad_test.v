module strings

fn test_pad_left() {
	assert pad_left('abc', 6, ' ') == '   abc'
	assert pad_left('abc', 6, '-') == '---abc'
	assert pad_left('abc', 3, '-') == 'abc'
	assert pad_left('abc', 6, 'xy') == 'xyxabc'
	assert pad_left('', 3, '-') == '---'
	assert pad_left('abc', 6, '') == 'abc'
}

fn test_pad_right() {
	assert pad_right('abc', 6, ' ') == 'abc   '
	assert pad_right('abc', 6, '-') == 'abc---'
	assert pad_right('abc', 6, 'xy') == 'abcxyx'
}

fn test_center() {
	assert center('abc', 7, ' ') == '  abc  '
	assert center('abc', 8, ' ') == '  abc   '
	assert center('abc', 6, '-') == '-abc--'
	assert center('abc', 3, '-') == 'abc'
}

fn test_zfill() {
	assert zfill('42', 5) == '00042'
	assert zfill('-42', 5) == '-0042'
	assert zfill('+42', 5) == '+0042'
	assert zfill('42', 2) == '42'
	assert zfill('', 3) == '000'
}

fn test_pad_unicode() {
	assert pad_left('😀', 3, '-') == '--😀'
	assert zfill('😀', 4) == '000😀'
}
