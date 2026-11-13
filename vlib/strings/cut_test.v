module strings

fn test_cut() {
	before, after, found := cut('a=b', '=')
	assert found
	assert before == 'a'
	assert after == 'b'
	b2, a2, f2 := cut('a=b=c', '=')
	assert f2
	assert b2 == 'a'
	assert a2 == 'b=c'
	b3, a3, f3 := cut('nope', '=')
	assert !f3
	assert b3 == 'nope'
	assert a3 == ''
}

fn test_cut_prefix() {
	s, ok := cut_prefix('abc', 'ab')
	assert ok
	assert s == 'c'
	s2, ok2 := cut_prefix('abc', 'x')
	assert !ok2
	assert s2 == 'abc'
}

fn test_cut_suffix() {
	s, ok := cut_suffix('abc', 'bc')
	assert ok
	assert s == 'a'
	s2, ok2 := cut_suffix('abc', 'x')
	assert !ok2
	assert s2 == 'abc'
}

fn test_cut_unicode() {
	// V strings are UTF-8 and indexing/slicing are byte-based, matching Go.
	before, after, found := cut('café=rest', '=')
	assert found
	assert before == 'café'
	assert after == 'rest'
	// multibyte separator
	b, a, f := cut('hello—world', '—')
	assert f
	assert b == 'hello'
	assert a == 'world'
	// absent separator with multibyte content
	b2, a2, f2 := cut('café', '=')
	assert !f2
	assert b2 == 'café'
	assert a2 == ''
	// unicode separator
	bu, au, fu := cut('αβγ—δε', '—')
	assert fu
	assert bu == 'αβγ'
	assert au == 'δε'
	// unicode separator absent
	bu2, au2, fu2 := cut('αβγδε', '—')
	assert !fu2
	assert bu2 == 'αβγδε'
	assert au2 == ''
	// empty separator
	be, ae, fe := cut('café', '')
	assert fe
	assert be == ''
	assert ae == 'café'
}

fn test_cut_prefix_unicode() {
	s, ok := cut_prefix('cafésuffix', 'café')
	assert ok
	assert s == 'suffix'
	s2, ok2 := cut_prefix('cafésuffix', 'cafe')
	assert !ok2
	assert s2 == 'cafésuffix'
}

fn test_cut_suffix_unicode() {
	s, ok := cut_suffix('prefixcafé', 'café')
	assert ok
	assert s == 'prefix'
	s2, ok2 := cut_suffix('prefixcafé', 'cafe')
	assert !ok2
	assert s2 == 'prefixcafé'
}

fn test_cut_empty_inputs_and_boundaries() {
	b0, a0, f0 := cut('', '')
	assert f0
	assert b0 == ''
	assert a0 == ''
	b1, a1, f1 := cut('', '=')
	assert !f1
	assert b1 == ''
	assert a1 == ''
	b2, a2, f2 := cut('abc', 'abc')
	assert f2
	assert b2 == ''
	assert a2 == ''
	b3, a3, f3 := cut('=abc=', '=')
	assert f3
	assert b3 == ''
	assert a3 == 'abc='
	b4, a4, f4 := cut('abc=', '=')
	assert f4
	assert b4 == 'abc'
	assert a4 == ''
	b5, a5, f5 := cut('abc', 'abcd')
	assert !f5
	assert b5 == 'abc'
	assert a5 == ''
}

fn test_cut_empty_affixes() {
	for s in ['', 'abc'] {
		without_prefix, has_prefix := cut_prefix(s, '')
		assert has_prefix
		assert without_prefix == s
		without_suffix, has_suffix := cut_suffix(s, '')
		assert has_suffix
		assert without_suffix == s
	}
}

fn test_cut_affixes_match_whole_strings_and_remove_once() {
	for s in ['abc', 'café'] {
		without_prefix, has_prefix := cut_prefix(s, s)
		assert has_prefix
		assert without_prefix == ''
		without_suffix, has_suffix := cut_suffix(s, s)
		assert has_suffix
		assert without_suffix == ''
		prefix_once, prefix_found := cut_prefix(s + s, s)
		assert prefix_found
		assert prefix_once == s
		suffix_once, suffix_found := cut_suffix(s + s, s)
		assert suffix_found
		assert suffix_once == s
		long_prefix, no_prefix := cut_prefix(s, s + s)
		assert !no_prefix
		assert long_prefix == s
		long_suffix, no_suffix := cut_suffix(s, s + s)
		assert !no_suffix
		assert long_suffix == s
	}
}

fn test_cut_preserves_binary_content() {
	s := [u8(0xff), 0, 0xfe].bytestr()
	before, after, found := cut(s, '\x00')
	assert found
	assert before.bytes() == [u8(0xff)]
	assert after.bytes() == [u8(0xfe)]
	without_prefix, has_prefix := cut_prefix(s, [u8(0xff), 0].bytestr())
	assert has_prefix
	assert without_prefix.bytes() == [u8(0xfe)]
	without_suffix, has_suffix := cut_suffix(s, [u8(0), 0xfe].bytestr())
	assert has_suffix
	assert without_suffix.bytes() == [u8(0xff)]
}
