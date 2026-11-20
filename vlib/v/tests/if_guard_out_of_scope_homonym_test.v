fn test_if_guard_ignores_out_of_scope_homonym() {
	m := {
		'a': 1
		'b': 2
	}
	for k in ['x'] {
		h := k + '!'
		assert h == 'x!'
	}
	// `h` below is a fresh guard binding of type `int`; the out-of-scope
	// `string h` from the loop above must not leak into the if-value type.
	n := if h := m['a'] {
		h
	} else {
		m['b'] or { -1 }
	}
	assert n == 1
	p := if h := m['missing'] {
		h
	} else {
		m['b'] or { -1 }
	}
	assert p == 2
}

fn test_nested_if_guard_with_out_of_scope_homonym() {
	m := {
		'a': 1
		'b': 2
	}
	for k in ['x'] {
		h := k + '!'
		assert h == 'x!'
	}
	q := if h := m['a'] {
		if g := m['b'] {
			g * 10
		} else {
			-1
		}
	} else {
		-1
	}
	assert q == 20
}
