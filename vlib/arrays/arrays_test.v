module arrays

fn test_min() {
	a := [8, 2, 6, 4]
	mut ri := min(a)!
	assert ri == 2
	ri = min(a[2..])!
	assert ri == 4
	b := [f32(5.1), 3.1, 1.1, 9.1]
	mut rf := min(b)!
	assert rf == f32(1.1)
	rf = min(b[..2])!
	assert rf == f32(3.1)
	c := [u8(4), 9, 3, 1]
	mut rb := min(c)!
	assert rb == u8(1)
	rb = min(c[..3])!
	assert rb == u8(3)
}

fn test_max() {
	a := [8, 2, 6, 4]
	mut ri := max(a)!
	assert ri == 8
	ri = max(a[1..])!
	assert ri == 6
	b := [f32(5.1), 3.1, 1.1, 9.1]
	mut rf := max(b)!
	assert rf == f32(9.1)
	rf = max(b[..3])!
	assert rf == f32(5.1)
	c := [u8(4), 9, 3, 1]
	mut rb := max(c)!
	assert rb == u8(9)
	rb = max(c[2..])!
	assert rb == u8(3)
}

fn test_idx_min() {
	a := [8, 2, 6, 4]
	ri := idx_min(a)!
	assert ri == 1
	b := [f32(5.1), 3.1, 1.1, 9.1]
	rf := idx_min(b)!
	assert rf == 2
	c := [u8(4), 9, 3, 1]
	rb := idx_min(c)!
	assert rb == 3
}

fn test_idx_max() {
	a := [8, 2, 6, 4]
	ri := idx_max(a)!
	assert ri == 0
	b := [f32(5.1), 3.1, 1.1, 9.1]
	rf := idx_max(b)!
	assert rf == 3
	c := [u8(4), 9, 3, 1]
	rb := idx_max(c)!
	assert rb == 1
}

fn test_merge() {
	a := [1, 3, 5, 5, 7]
	b := [2, 4, 4, 5, 6, 8]
	c := []int{}
	d := []int{}
	assert merge[int](a, b) == [1, 2, 3, 4, 4, 5, 5, 5, 6, 7, 8]
	assert merge[int](c, d) == []
	assert merge[int](a, c) == a
	assert merge[int](d, b) == b
}

fn test_append() {
	a := [1, 3, 5, 5, 7]
	b := [2, 4, 4, 5, 6, 8]
	assert append(a, b) == [1, 3, 5, 5, 7, 2, 4, 4, 5, 6, 8]
	assert append(b, a) == [2, 4, 4, 5, 6, 8, 1, 3, 5, 5, 7]
	assert append(a, []int{}) == a
	assert append([]int{}, b) == b
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

fn test_array_flat_map() {
	a := ['Hello V', 'Hello World', 'V Lang']
	assert flat_map[string, string](a, fn (e string) []string {
		return e.split(' ')
	}) == ['Hello', 'V', 'Hello', 'World', 'V', 'Lang']
}

fn test_array_flat_map_indexed() {
	a := ['AB', 'CD', 'EF']
	assert flat_map_indexed[string, string](a, fn (i int, e string) []string {
		mut arr := [i.str()]
		arr << e.split('')
		return arr
	}) == ['0', 'A', 'B', '1', 'C', 'D', '2', 'E', 'F']
}

fn test_map_indexed() {
	a := [1, 2, 3]
	assert map_indexed[int, int](a, fn (i int, e int) int {
		return i + e * e
	}) == [1, 5, 11]
}

fn test_group() {
	x := [4, 5, 6]
	y := [2, 1, 3]

	z := group[int](x, y)
	assert z == [[4, 2], [5, 1], [6, 3]]
	x2 := [8, 9]
	z2 := group[int](x2, y)
	assert z2 == [[8, 2], [9, 1]]
	assert group[int](x, []int{}) == [][]int{}
}

fn test_chunk() {
	x := [1, 2, 3, 4, 5]
	y := ['a', 'b', 'c', 'd', 'e', 'f']

	z1 := chunk[int](x, 2)
	assert z1 == [[1, 2], [3, 4], [5]]
	z2 := chunk[string](y, 3)
	assert z2 == [['a', 'b', 'c'], ['d', 'e', 'f']]
	assert chunk[int]([]int{}, 2) == [][]int{}
}

fn test_chunk_while() {
	assert chunk_while([0, 9, 2, 2, 3, 2, 7, 5, 9, 5], fn (x int, y int) bool {
		return x == y
	}) == [[0], [9], [2, 2], [3], [2], [7], [5], [9], [5]]

	assert chunk_while([0, 9, 2, 2, 3, 2, 7, 5, 9, 5], fn (x int, y int) bool {
		return x <= y
	}) == [[0, 9], [2, 2, 3], [2, 7], [5, 9], [5]]

	assert chunk_while('aaaabbbcca'.runes(), fn (x rune, y rune) bool {
		return x == y
	}).map({
		it[0]: it.len
	}).str() == '[{`a`: 4}, {`b`: 3}, {`c`: 2}, {`a`: 1}]'
}

fn test_window() {
	x := [1, 2, 3, 4, 5, 6]

	assert window[int](x, size: 3) == [[1, 2, 3], [2, 3, 4], [3, 4, 5],
		[4, 5, 6]]
	assert window[int](x, size: 3, step: 2) == [[1, 2, 3], [3, 4, 5]]
	assert window[int]([]int{}, size: 2) == [][]int{}
}

/////////////////////////////
type MyAlias = i64

fn (a MyAlias) + (b MyAlias) MyAlias {
	return MyAlias(i64(a) * 10 + i64(b) * 10)
}

struct MyStruct {
	x int = 5
}

fn (a MyStruct) + (b MyStruct) MyStruct {
	return MyStruct{
		x: a.x + b.x
	}
}

fn test_sum() {
	assert sum([1, 2, 3, 4, 5]) or { 0 } == 15
	assert sum([1.0, 2.5, 3.5, 4.0]) or { 0.0 } == 11.0
	assert sum([]int{}) or { 0 } == 0
	// test summing of a struct, with an overloaded + operator:
	s := [MyStruct{
		x: 30
	}, MyStruct{
		x: 20
	}, MyStruct{
		x: 10
	}]
	assert sum(s)! == MyStruct{
		x: 60
	}
	// test summing of a number type alias with an overloaded + operator:
	assert sum([MyAlias(5), MyAlias(7), MyAlias(3)])! == MyAlias((5 * 10 + 7 * 10) * 10 + 3 * 10)
}

/////////////////////////////
fn test_reduce() {
	x := [1, 2, 3, 4, 5]

	assert reduce[int](x, fn (t1 int, t2 int) int {
		return t1 + t2
	}) or { 0 } == 15
	assert reduce[string](['H', 'e', 'l', 'l', 'o'], fn (t1 string, t2 string) string {
		return t1 + t2
	}) or { '' } == 'Hello' // For the sake please use array's join instead.
	assert reduce[int]([]int{}, fn (t1 int, t2 int) int {
		return 0
	}) or { -1 } == -1
}

fn test_reduce_indexed() {
	x := [1, 2, 3, 4, 5]
	assert reduce_indexed[int](x, fn (idx int, t1 int, t2 int) int {
		return idx + t1 + t2
	}) or { 0 } == 25
}

fn test_filter_indexed() {
	x := [0, 1, 2, 3, 4, 5]

	assert filter_indexed[int](x, fn (idx int, e int) bool {
		return idx % 2 == 0
	}) == [0, 2, 4]
}

fn test_fold() {
	x := [1, 2, 3, 4, 5]

	assert fold[int, int](x, 5, fn (r int, t int) int {
		return r + t
	}) == 20
	assert fold[string, int](['H', 'e', 'l', 'l', 'l'], 0, fn (r int, t string) int {
		return r + t[0]
	}) == 497
	assert fold[int, int]([]int{}, -1, fn (t1 int, t2 int) int {
		return 0
	}) == -1
}

fn test_fold_indexed() {
	x := [1, 2, 3, 4, 5]

	assert fold_indexed[int, int](x, 5, fn (idx int, r int, t int) int {
		return idx + r + t
	}) == 30
}

fn test_flatten() {
	x := [[1, 2, 3], [4, 5, 6]]

	assert flatten[int](x) == [1, 2, 3, 4, 5, 6]
	assert flatten[int]([[]int{}]) == []
}

fn test_group_by() {
	x := ['H', 'el', 'l', 'o ']

	assert group_by[int, string](x, fn (v string) int {
		return v.len
	}) == {
		1: ['H', 'l']
		2: ['el', 'o ']
	}
	assert group_by[int, int]([]int{}, fn (v int) int {
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

fn test_binary_search() {
	a := [1, 3, 3, 4, 5, 6, 7, 8, 10]
	assert binary_search(a, 3)! == 1
	assert (binary_search(a, 0) or { -1 }) == -1
}

fn test_lower_bound() {
	a := [1, 3, 3, 4, 5, 6, 7, 8, 10]
	b := []int{}
	c := [1, 2, 3]
	assert lower_bound(a, 2)! == 3
	assert (lower_bound(b, 4) or { -1 }) == -1
	assert lower_bound(c, 3)! == 3
}

fn test_upper_bound() {
	a := [1, 3, 3, 4, 5, 6, 7, 8, 10]
	b := []int{}
	c := [1, 2, 3]
	assert upper_bound(a, 9)! == 8
	assert (upper_bound(b, 4) or { -1 }) == -1
	assert upper_bound(c, 2)! == 2
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
	assert can_copy_bits[u8]()
	assert can_copy_bits[int]()
	assert can_copy_bits[voidptr]()
	assert can_copy_bits[&u8]()
	// autofree needs to intercept assign
	assert !can_copy_bits[string]()
	assert !can_copy_bits[[]int]()
	// map not copyable
	assert !can_copy_bits[map[string]int]()
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
	array := [XYZ{}]!
	assert (XYZ{} in names) == true
}

fn test_index_of_first() {
	// vfmt off
	assert index_of_first([1], fn (idx int, x int) bool { return x == 0 }) == -1
	assert index_of_first([4, 5, 0, 7, 0, 9], fn (idx int, x int) bool { return x == 0 }) == 2
	assert index_of_first([4, 5, 0, 7, 0, 9], fn (idx int, x int) bool { return x == 4 }) == 0
	// vfmt on
}

fn test_index_of_last() {
	// vfmt off
	assert index_of_last([1], fn (idx int, x int) bool { return x == 0 }) == -1
	assert index_of_last([4, 5, 0, 7, 0, 9], fn (idx int, x int) bool { return x == 0 }) == 4
	assert index_of_last([4, 5, 0, 7, 0, 9], fn (idx int, x int) bool { return x == 4 }) == 0
	// vfmt on
}

fn test_map_of_indexes() {
	// vfmt off
	assert arrays.map_of_indexes([]int{}) == {}
	assert arrays.map_of_indexes([1]) == {1: [0]}
	assert arrays.map_of_indexes([1, 2, 3, 999]) == {1: [0], 2: [1], 3: [2], 999: [3]}
	assert arrays.map_of_indexes([999, 1, 2, 3]) == {1: [1], 2: [2], 3: [3], 999: [0]}
	assert arrays.map_of_indexes([1, 2, 3, 4, 4, 2, 1, 4, 4, 999]) == {1: [0, 6], 2: [1, 5], 3: [2], 4: [3, 4, 7, 8], 999: [9]}

	assert arrays.map_of_indexes([]string{}) == {}
	assert arrays.map_of_indexes(['abc']) == {'abc': [0]}
	assert arrays.map_of_indexes(['abc', 'abc']) == {'abc': [0, 1]}
	assert arrays.map_of_indexes(['abc', 'def', 'abc']) == {'abc': [0, 2], 'def': [1]}
	// vfmt on
}

fn test_map_of_counts() {
	// vfmt off
	assert map_of_counts([]int{}) == {}
	assert map_of_counts([1]) == {1: 1}
	assert map_of_counts([1, 2, 3, 999]) == {1: 1, 2: 1, 3: 1, 999: 1}
	assert map_of_counts([999, 1, 2, 3]) == {1: 1, 2: 1, 3: 1, 999: 1}
	assert map_of_counts([1, 2, 3, 4, 4, 2, 1, 4, 4, 999]) == {1: 2, 2: 2, 3: 1, 4: 4, 999: 1}

	assert map_of_counts([]string{}) == {}
	assert map_of_counts(['abc']) == {'abc': 1}
	assert map_of_counts(['abc', 'abc']) == {'abc': 2}
	assert map_of_counts(['abc', 'def', 'abc']) == {'abc': 2, 'def': 1}
	// vfmt on
}

struct FindTest {
	name string
	age  int
}

const test_structs = [FindTest{'one', 1}, FindTest{'two', 2},
	FindTest{'three', 3}, FindTest{'one', 4}]

fn test_find_first() {
	// element in array
	a := [1, 2, 3, 4, 5]
	assert find_first[int](a, fn (arr int) bool {
		return arr == 3
	})? == 3, 'find element couldnt find the right element'

	// find struct
	find_by_name := find_first(test_structs, fn (arr FindTest) bool {
		return arr.name == 'one'
	})?
	assert find_by_name == FindTest{'one', 1}

	// not found
	if _ := find_first(test_structs, fn (arr FindTest) bool {
		return arr.name == 'nothing'
	})
	{
		assert false
	} else {
		assert true
	}
}

fn test_find_last() {
	// // element in array
	a := [1, 2, 3, 4, 5]
	assert find_last[int](a, fn (arr int) bool {
		return arr == 3
	})? == 3, 'find element couldnt find the right element'

	// find struct
	find_by_name := find_last(test_structs, fn (arr FindTest) bool {
		return arr.name == 'one'
	})?
	assert find_by_name == FindTest{'one', 4}

	// not found
	if _ := find_last(test_structs, fn (arr FindTest) bool {
		return arr.name == 'nothing'
	})
	{
		assert false
	} else {
		assert true
	}
}

fn test_join_to_string() {
	assert join_to_string[FindTest](test_structs, ':', fn (it FindTest) string {
		return it.name
	}) == 'one:two:three:one'
	assert join_to_string[FindTest](test_structs, '', fn (it FindTest) string {
		return it.name
	}) == 'onetwothreeone'
	assert join_to_string[int]([]int{}, ':', fn (it int) string {
		return '1'
	}) == ''
}

fn test_partition() {
	a := [1, 2, 3, 4, 5, 6, 7, 8]
	lower, upper := partition(a, fn (it int) bool {
		return it < 5
	})
	assert lower.len == 4
	assert upper.len == 4
	assert lower == [1, 2, 3, 4]
	assert upper == [5, 6, 7, 8]

	lower2, upper2 := partition(a, fn (it int) bool {
		return it < 1
	})
	assert lower2.len == 0
	assert upper2.len == 8
}

fn test_each() {
	a := [99, 1, 2, 3, 4, 5, 6, 7, 8, 1001]
	mut control_sum := 0
	for x in a {
		control_sum += x
	}

	each(a, fn (x int) {
		println(x)
	})
	mut sum := 0
	mut psum := &sum
	each(a, fn [mut psum] (x int) {
		unsafe {
			*psum += x
		}
	})
	assert control_sum == sum
}

fn test_each_indexed() {
	a := [99, 1, 2, 3, 4, 5, 6, 7, 8, 1001]
	mut control_sum := 0
	f := fn (idx int, x int) int {
		return (idx + 1) * 1000_000 + x
	}
	for idx, x in a {
		control_sum += f(idx, x)
	}

	each_indexed(a, fn (idx int, x int) {
		println('idx: ${idx}, x: ${x}')
	})
	mut sum := 0
	mut psum := &sum
	each_indexed(a, fn [mut psum, f] (idx int, x int) {
		unsafe {
			*psum += f(idx, x)
		}
	})
	assert control_sum == sum
}
