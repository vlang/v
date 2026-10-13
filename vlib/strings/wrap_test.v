module strings

fn test_wrap_basic() {
	assert wrap('the quick brown fox jumps', 10) == 'the quick\nbrown fox\njumps'
}

fn test_wrap_long_word_breaks() {
	assert wrap('abcdefghij', 4) == 'abcd\nefgh\nij'
}

fn test_wrap_is_rune_aware() {
	// 'héllo' is 5 runes but 6 bytes: a byte-based wrap at 6 would split 'é'.
	assert wrap('héllo wörld', 6) == 'héllo\nwörld'
}

fn test_wrap_keeps_explicit_newlines() {
	assert wrap('a\nb', 10) == 'a\nb'
	assert wrap('', 10) == ''
	assert wrap('abc', 0) == 'abc'
}

fn test_indent() {
	assert indent('a\nb', '> ', false) == '> a\n> b'
	assert indent('a\nb', '> ', true) == 'a\n> b'
}

fn test_dedent() {
	assert dedent('  a\n    b\n  c') == 'a\n  b\nc'
	assert dedent('  a\n\n  c') == 'a\n\nc'
	assert dedent('ab\ncd') == 'ab\ncd'
}
