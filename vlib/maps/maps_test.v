module maps

fn test_filter() {
	m1 := {
		0: 'ab'
		1: 'bc'
		2: 'cd'
		3: 'de'
		4: 'ef'
		5: 'fg'
	}
	assert filter(m1, fn (k int, v string) bool {
		return k % 2 == 0
	}) == {
		0: 'ab'
		2: 'cd'
		4: 'ef'
	}
	assert filter(m1, fn (k int, v string) bool {
		return v.contains('b') || v.contains('c')
	}) == {
		0: 'ab'
		1: 'bc'
		2: 'cd'
	}
}

fn test_maps() {
	m1 := {
		`a`: 'bc'
		`d`: 'ef'
		`g`: 'hi'
	}
	assert maps<rune, string, rune, string>(m1, fn (k rune, v string) (rune, string) {
		return k, '$k$v'
	}) == {
		`a`: 'abc'
		`d`: 'def'
		`g`: 'ghi'
	}
}
