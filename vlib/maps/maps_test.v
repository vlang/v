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
		return '${k}${v}'
	}) == ['abc', 'def', 'ghi']
}

fn test_flat_map() {
	m1 := {
		1: [2, 3]
		4: [5, 6]
		7: [8, 9]
	}
	assert flat_map[int, []int, int](m1, fn (k int, v []int) []int {
		mut a := [k]
		a << v
		return a
	}) == [1, 2, 3, 4, 5, 6, 7, 8, 9]
}

fn test_to_map() {
	m1 := {
		0: '0'
		1: '1'
		2: '2'
		3: '3'
		4: '4'
		5: '5'
	}
	assert to_map[int, string, string, int](m1, fn (k int, v string) (string, int) {
		return v, k
	}) == {
		'0': 0
		'1': 1
		'2': 2
		'3': 3
		'4': 4
		'5': 5
	}
}

fn test_invert() {
	m1 := {
		0: '0'
		1: '1'
		2: '2'
		3: '3'
		4: '4'
		5: '5'
	}
	assert invert(m1) == {
		'0': 0
		'1': 1
		'2': 2
		'3': 3
		'4': 4
		'5': 5
	}
}

fn test_from_array() {
	a1 := [
		'a',
		'b',
		'c',
		'd',
		'e',
		'f',
	]
	assert from_array(a1) == {
		0: 'a'
		1: 'b'
		2: 'c'
		3: 'd'
		4: 'e'
		5: 'f'
	}
}

fn test_merge_in_place() {
	mut m1 := {
		'abc': 'def'
		'aa':  'bb'
	}
	m2 := {
		'xyz': 'zyx'
		'aa':  'dd'
	}
	merge_in_place(mut m1, m2)
	assert m1 == {
		'abc': 'def'
		'aa':  'dd'
		'xyz': 'zyx'
	}
	assert m2 == {
		'xyz': 'zyx'
		'aa':  'dd'
	}

	mut im1 := {
		11: 22
		33: 44
	}
	im2 := {
		55: 66
		33: 999
	}
	merge_in_place(mut im1, im2)
	assert im1 == {
		11: 22
		33: 999
		55: 66
	}
	assert im2 == {
		55: 66
		33: 999
	}
}

fn test_merge() {
	m1 := {
		'abc': 'def'
		'aa':  'bb'
	}
	m2 := {
		'xyz': 'zyx'
		'aa':  'dd'
	}
	res := merge(m1, m2)
	assert res == {
		'abc': 'def'
		'aa':  'dd'
		'xyz': 'zyx'
	}
	assert m1 == {
		'abc': 'def'
		'aa':  'bb'
	}
	assert m2 == {
		'xyz': 'zyx'
		'aa':  'dd'
	}

	mut im1 := {
		11: 22
		33: 44
	}
	im2 := {
		55: 66
		33: 999
	}
	ires := merge(im1, im2)
	assert im1 == {
		11: 22
		33: 44
	}
	assert im2 == {
		55: 66
		33: 999
	}
	assert ires == {
		11: 22
		33: 999
		55: 66
	}
}
