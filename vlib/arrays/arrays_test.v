module arrays

fn test_min() ? {
	a := [8, 2, 6, 4]
	mut ri := min(a)?
	assert ri == 2
	ri = min(a[2..])?
	assert ri == 4
	b := [f32(5.1), 3.1, 1.1, 9.1]
	mut rf := min(b)?
	assert rf == f32(1.1)
	rf = min(b[..2])?
	assert rf == f32(3.1)
	c := [u8(4), 9, 3, 1]
	mut rb := min(c)?
	assert rb == u8(1)
	rb = min(c[..3])?
	assert rb == u8(3)
}

fn test_max() ? {
	a := [8, 2, 6, 4]
	mut ri := max(a)?
	assert ri == 8
	ri = max(a[1..])?
	assert ri == 6
	b := [f32(5.1), 3.1, 1.1, 9.1]
	mut rf := max(b)?
	assert rf == f32(9.1)
	rf = max(b[..3])?
	assert rf == f32(5.1)
	c := [u8(4), 9, 3, 1]
	mut rb := max(c)?
	assert rb == u8(9)
	rb = max(c[2..])?
	assert rb == u8(3)
}

fn test_idx_min() ? {
	a := [8, 2, 6, 4]
	ri := idx_min(a)?
	assert ri == 1
	b := [f32(5.1), 3.1, 1.1, 9.1]
	rf := idx_min(b)?
	assert rf == 2
	c := [u8(4), 9, 3, 1]
	rb := idx_min(c)?
	assert rb == 3
}

fn test_idx_max() ? {
	a := [8, 2, 6, 4]
	ri := idx_max(a)?
	assert ri == 0
	b := [f32(5.1), 3.1, 1.1, 9.1]
	rf := idx_max(b)?
	assert rf == 3
	c := [u8(4), 9, 3, 1]
	rb := idx_max(c)?
	assert rb == 1
}

fn test_merge() {
	a := [1, 3, 5, 5, 7]
	b := [2, 4, 4, 5, 6, 8]
	c := []int{}
	d := []int{}
	assert merge<int>(a, b) == [1, 2, 3, 4, 4, 5, 5, 5, 6, 7, 8]
	assert merge<int>(c, d) == []
	assert merge<int>(a, c) == a
	assert merge<int>(d, b) == b
}

fn test_fixed_array_assignment() {
	mut a := [2]int{}
	a[0] = 111
	a[1] = 222
	b := a
	assert b[0] == a[0]
	assert b[1] == a[1]
	mut c := [2]int{}
	c = a
	assert c[0] == a[0]
	assert c[1] == a[1]
	d := [3]int{init: 333}
	for val in d {
		assert val == 333
	}
	e := [3]string{init: 'vlang'}
	for val in e {
		assert val == 'vlang'
	}
}

fn test_group() {
	x := [4, 5, 6]
	y := [2, 1, 3]

	z := group<int>(x, y)
	assert z == [[4, 2], [5, 1], [6, 3]]
	x2 := [8, 9]
	z2 := group<int>(x2, y)
	assert z2 == [[8, 2], [9, 1]]
	assert group<int>(x, []int{}) == [][]int{}
}

fn test_chunk() {
	x := [1, 2, 3, 4, 5]
	y := ['a', 'b', 'c', 'd', 'e', 'f']

	z1 := chunk<int>(x, 2)
	assert z1 == [[1, 2], [3, 4], [5]]
	z2 := chunk<string>(y, 3)
	assert z2 == [['a', 'b', 'c'], ['d', 'e', 'f']]
	assert chunk<int>([]int{}, 2) == [][]int{}
}

fn test_window() {
	x := [1, 2, 3, 4, 5, 6]

	assert window<int>(x, size: 3) == [[1, 2, 3], [2, 3, 4], [3, 4, 5],
		[4, 5, 6]]
	assert window<int>(x, size: 3, step: 2) == [[1, 2, 3], [3, 4, 5]]
	assert window<int>([]int{}, size: 2) == [][]int{}
}

fn test_sum() {
	x := [1, 2, 3, 4, 5]

	assert sum<int>(x) or { 0 } == 15
	assert sum<f64>([1.0, 2.5, 3.5, 4.0]) or { 0 } == 11.0
	assert sum<int>([]int{}) or { 0 } == 0
}

fn test_reduce() {
	x := [1, 2, 3, 4, 5]

	assert reduce<int>(x, fn (t1 int, t2 int) int {
		return t1 + t2
	}) or { 0 } == 15
	assert reduce<string>(['H', 'e', 'l', 'l', 'o'], fn (t1 string, t2 string) string {
		return t1 + t2
	}) or { '' } == 'Hello' // For the sake please use array's join instead.
	assert reduce<int>([]int{}, fn (t1 int, t2 int) int {
		return 0
	}) or { -1 } == -1
}

fn test_fold() {
	x := [1, 2, 3, 4, 5]

	assert fold<int, int>(x, 5, fn (r int, t int) int {
		return r + t
	}) == 20
	assert fold<string, int>(['H', 'e', 'l', 'l', 'l'], 0, fn (r int, t string) int {
		return r + t[0]
	}) == 497
	assert fold<int, int>([]int{}, -1, fn (t1 int, t2 int) int {
		return 0
	}) == -1
}

fn test_flatten() {
	x := [[1, 2, 3], [4, 5, 6]]

	assert flatten<int>(x) == [1, 2, 3, 4, 5, 6]
	assert flatten<int>([[]int{}]) == []
}

fn test_group_by() {
	x := ['H', 'el', 'l', 'o ']

	assert group_by<int, string>(x, fn (v string) int {
		return v.len
	}) == {
		1: ['H', 'l']
		2: ['el', 'o ']
	}
	assert group_by<int, int>([]int{}, fn (v int) int {
		return 0
	}) == map[int][]int{}
}

fn test_concat_int() {
	mut a := [1, 2, 3]
	mut b := [3, 2, 1]

	assert concat(a, ...b) == [1, 2, 3, 3, 2, 1]
}

fn test_concat_string() {
	mut a := ['1', '2', '3']
	mut b := ['3', '2', '1']

	assert concat(a, ...b) == ['1', '2', '3', '3', '2', '1']
}

fn test_binary_search() ? {
	a := [1, 3, 3, 4, 5, 6, 7, 8, 10]
	assert binary_search(a, 3)? == 1
	assert (binary_search(a, 0) or { -1 }) == -1
}

fn test_lower_bound() ? {
	a := [1, 3, 3, 4, 5, 6, 7, 8, 10]
	b := []int{}
	c := [1, 2, 3]
	assert lower_bound(a, 2)? == 3
	assert (lower_bound(b, 4) or { -1 }) == -1
	assert lower_bound(c, 3)? == 3
}

fn test_upper_bound() ? {
	a := [1, 3, 3, 4, 5, 6, 7, 8, 10]
	b := []int{}
	c := [1, 2, 3]
	assert upper_bound(a, 9)? == 8
	assert (upper_bound(b, 4) or { -1 }) == -1
	assert upper_bound(c, 2)? == 2
}

fn test_rotate_right() {
	mut x := [1, 2, 3, 4, 5, 6]
	rotate_right(mut x, 2)
	assert x == [5, 6, 1, 2, 3, 4]
}

fn test_rotate_left() {
	mut x := [1, 2, 3, 4, 5, 6]
	rotate_left(mut x, 2)
	assert x == [3, 4, 5, 6, 1, 2]
}

struct Abc {
	x u64 = 1
	y u64 = 2
	z u64 = 3
}

fn test_rotate_right_struct() {
	mut x := [Abc{1, 0, 1}, Abc{2, 0, 1}, Abc{3, 0, 1}, Abc{4, 0, 1},
		Abc{5, 0, 1}, Abc{6, 0, 1}]
	rotate_right(mut x, 2)
	assert x == [Abc{5, 0, 1}, Abc{6, 0, 1}, Abc{1, 0, 1}, Abc{2, 0, 1},
		Abc{3, 0, 1}, Abc{4, 0, 1}]
}

fn test_rotate_left_struct() {
	mut x := [Abc{1, 0, 1}, Abc{2, 0, 1}, Abc{3, 0, 1}, Abc{4, 0, 1},
		Abc{5, 0, 1}, Abc{6, 0, 1}]
	rotate_left(mut x, 2)
	assert x == [Abc{3, 0, 1}, Abc{4, 0, 1}, Abc{5, 0, 1}, Abc{6, 0, 1},
		Abc{1, 0, 1}, Abc{2, 0, 1}]
}

fn test_rotate_right_string() {
	mut x := ['x1', 'x2', 'x3', 'x4', 'x5', 'x6']
	rotate_right(mut x, 2)
	assert x == ['x5', 'x6', 'x1', 'x2', 'x3', 'x4']
}

fn test_rotate_left_string() {
	mut x := ['x1', 'x2', 'x3', 'x4', 'x5', 'x6']
	rotate_left(mut x, 2)
	assert x == ['x3', 'x4', 'x5', 'x6', 'x1', 'x2']
}

fn test_copy() {
	mut a := [1, 2, 3]
	mut b := [4, 5, 6]
	assert copy(mut b, a) == 3
	assert b == [1, 2, 3]
	// check independent copies
	b[0] = 99
	assert a[0] == 1
	// check longer src
	b << 7
	assert copy(mut a, b) == 3
	assert a == [99, 2, 3]
	// check longer dst
	assert copy(mut b, [8, 9]) == 2
	assert b == [8, 9, 3, 7]
}

fn test_can_copy_bits() {
	assert can_copy_bits<u8>()
	assert can_copy_bits<int>()
	assert can_copy_bits<voidptr>()
	assert can_copy_bits<&u8>()
	// autofree needs to intercept assign
	assert !can_copy_bits<string>()
	assert !can_copy_bits<[]int>()
	// map not copyable
	assert !can_copy_bits<map[string]int>()
}

type Str = string

fn test_alias_string_contains() {
	names := [Str('')]
	assert (Str('') in names) == true
}

struct XYZ {}

fn test_array_append_empty_struct() {
	mut names := []XYZ{cap: 2}
	names << XYZ{}
	assert (XYZ{} in names) == true

	// test fixed array
	array := [XYZ{}]
	assert (XYZ{} in names) == true
}
