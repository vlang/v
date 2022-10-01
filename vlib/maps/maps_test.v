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

fn test_to_array() {
	m1 := {
		`a`: 'bc'
		`d`: 'ef'
		`g`: 'hi'
	}
	assert to_array(m1, fn (k rune, v string) string {
		return '$k$v'
	}) == ['abc', 'def', 'ghi']
}

fn test_to_array_and_flatten() {
	m1 := {
		1: [2, 3]
		4: [5, 6]
		7: [8, 9]
	}
	assert to_array_and_flatten<int, []int, int>(m1, fn (k int, v []int) []int {
		mut a := [k]
		a << v
		return a
	}) == [1, 2, 3, 4, 5, 6, 7, 8, 9]
}
