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
