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

fn test_padding_nonpositive_and_short_widths() {
	for width in [-10, 0, 1, 2] {
		assert pad_left('é😀', width, '界') == 'é😀'
		assert pad_right('é😀', width, '界') == 'é😀'
		assert center('é😀', width, '界') == 'é😀'
		assert zfill('é😀', width) == 'é😀'
	}
	assert pad_right('abc', 8, '') == 'abc'
	assert center('abc', 8, '') == 'abc'
	assert pad_left('', 0, '-') == ''
	assert pad_right('', -1, '-') == ''
	assert center('', 0, '-') == ''
	assert zfill('', -1) == ''
}

fn test_padding_repeats_complete_unicode_runes() {
	assert pad_left('é', 4, '界😀') == '界😀界é'
	assert pad_right('é', 4, '界😀') == 'é界😀界'
	assert center('é', 6, '界😀') == '界😀é界😀界'
	assert pad_right('', 3, '界😀') == '界😀界'
	assert center('', 3, '界😀') == '界界😀'
}

fn test_center_extra_padding_is_always_on_the_right() {
	assert center('ab', 5, '-') == '-ab--'
	assert center('abc', 6, '-') == '-abc--'
	assert center('ab', 3, 'xy') == 'abx'
}

fn test_zfill_sign_and_unicode() {
	assert zfill('+', 3) == '+00'
	assert zfill('-', 3) == '-00'
	assert zfill('-é😀', 5) == '-00é😀'
	assert zfill('+é😀', 5) == '+00é😀'
	assert zfill('−42', 5) == '00−42'
	assert zfill(' -42', 6) == '00 -42'
	assert zfill('--42', 6) == '-00-42'
}

fn test_zfill_preserves_body_bytes_after_sign() {
	for sign in [u8(`+`), u8(`-`)] {
		input := [sign, u8(0xff), u8(0)].bytestr()
		expected := [sign, u8(`0`), u8(`0`), u8(0xff), u8(0)].bytestr()
		assert zfill(input, 5) == expected
	}
}
