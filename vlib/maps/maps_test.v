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
	assert maps(m1, fn (k rune, v string) string {
		return '$k$v'
	}) == ['abc', 'def', 'ghi']
}

fn test_flat_map() {
	m1 := {
		1: [2, 3]
		4: [5, 6]
		7: [8, 9]
	}
	assert flat_map<int, []int, int>(m1, fn (k int, v []int) []int {
		mut a := [k]
		a << v
		return a
	}) == [1, 2, 3, 4, 5, 6, 7, 8, 9]
}
