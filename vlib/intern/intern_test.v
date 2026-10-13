module intern

fn test_intern_dedups() {
	mut p := new_pool()
	a := p.intern('hello')
	b := p.intern('hello')
	c := p.intern('world')
	assert p.len() == 2, 'equal strings must share one canonical copy'
	assert a == 'hello' && b == 'hello' && c == 'world'
	assert eq(a, b), 'same canonical storage must compare equal'
	assert eq(a, 'hello'), 'interned vs plain literal must compare equal'
	assert !eq(a, c)
	assert !eq('', 'x')
	e1 := p.intern('')
	e2 := p.intern('')
	assert eq(e1, e2), 'empty strings must intern cleanly'
	assert p.len() == 3
}

fn test_eq_fallback() {
	assert eq('abc', 'abc'), 'non-interned equals must hold'
	assert !eq('abc', 'abd')
	assert eq('', '')
	// long strings defeat any prefix shortcut: differ only at the end
	s1 := 'x'.repeat(1000) + 'a'
	s2 := 'x'.repeat(1000) + 'b'
	assert !eq(s1, s2)
	assert eq(s1, 'x'.repeat(1000) + 'a')
}
