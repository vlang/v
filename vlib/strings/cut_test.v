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
