module bytes

fn test_cut() {
	before, after, found := cut('a,b,c'.bytes(), ','.bytes())
	assert found
	assert before.bytestr() == 'a'
	assert after.bytestr() == 'b,c'
	b2, a2, f2 := cut('abc'.bytes(), 'z'.bytes())
	assert !f2
	assert b2.bytestr() == 'abc'
	assert a2.len == 0
}

fn test_cut_prefix() {
	s, ok := cut_prefix('foobar'.bytes(), 'foo'.bytes())
	assert ok
	assert s.bytestr() == 'bar'
	s2, ok2 := cut_prefix('foobar'.bytes(), 'baz'.bytes())
	assert !ok2
	assert s2.bytestr() == 'foobar'
}

fn test_cut_suffix() {
	s, ok := cut_suffix('foobar'.bytes(), 'bar'.bytes())
	assert ok
	assert s.bytestr() == 'foo'
	s2, ok2 := cut_suffix('foobar'.bytes(), 'baz'.bytes())
	assert !ok2
	assert s2.bytestr() == 'foobar'
}
