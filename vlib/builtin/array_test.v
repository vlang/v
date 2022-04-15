fn test_pointer() {
	mut arr := []&int{}
	a := 1
	b := 2
	c := 3
	arr << &a
	arr << &b
	arr << &c
	assert *arr[0] == 1
	arr[1] = &c
	assert *arr[1] == 3
	mut d_arr := [arr] // [][]&int
	d_arr << arr
	assert *d_arr[0][1] == 3
	println(*d_arr[0][1])
	assert *d_arr[1][0] == 1
}

fn test_assign() {
	mut arr := [2, 4, 8, 16, 32, 64, 128]
	arr[0] = 2
	arr[1] &= 255
	arr[2] |= 255
	arr[3] <<= 4
	arr[4] >>= 4
	arr[5] %= 5
	arr[6] ^= 3
	assert arr[0] == 2
	assert arr[1] == 4 & 255
	assert arr[2] == 8 | 255
	assert arr[3] == 16 << 4
	assert arr[4] == 32 >> 4
	assert arr[5] == 64 % 5
	assert arr[6] == 128 ^ 3
}

fn test_ints() {
	mut a := [1, 5, 2, 3]
	assert a.len == 4
	assert a[0] == 1
	assert a[2] == 2
	assert a.last() == 3
	a << 4
	assert a.len == 5
	assert a[4] == 4
	assert a.last() == 4
	s := a.str()
	assert s == '[1, 5, 2, 3, 4]'
	assert a[1] == 5
	assert a.last() == 4
}

fn test_deleting() {
	mut a := [1, 5, 2, 3, 4]
	assert a.len == 5
	assert a.str() == '[1, 5, 2, 3, 4]'
	a.delete(0)
	assert a.str() == '[5, 2, 3, 4]'
	assert a.len == 4
	a.delete(1)
	assert a.str() == '[5, 3, 4]'
	assert a.len == 3
	a.delete(a.len - 1)
	assert a.str() == '[5, 3]'
	assert a.len == 2
}

fn test_slice_delete() {
	mut a := [1.5, 2.5, 3.25, 4.5, 5.75]
	b := a[2..4]
	a.delete(0)
	assert a == [2.5, 3.25, 4.5, 5.75]
	assert b == [3.25, 4.5]
	a = [3.75, 4.25, -1.5, 2.25, 6.0]
	c := a[..3]
	a.delete(2)
	assert a == [3.75, 4.25, 2.25, 6.0]
	assert c == [3.75, 4.25, -1.5]
}

fn test_delete_many() {
	mut a := [1, 2, 3, 4, 5, 6, 7, 8, 9]
	b := a[2..6]
	a.delete_many(4, 3)
	assert a == [1, 2, 3, 4, 8, 9]
	assert b == [3, 4, 5, 6]
	c := a[..a.len]
	a.delete_many(2, 0) // this should just clone
	a[1] = 17
	assert a == [1, 17, 3, 4, 8, 9]
	assert c == [1, 2, 3, 4, 8, 9]
	a.delete_many(0, a.len)
	assert a == []int{}
}

fn test_short() {
	a := [1, 2, 3]
	assert a.len == 3
	assert a.cap == 3
	assert a[0] == 1
	assert a[1] == 2
	assert a[2] == 3
}

fn test_large() {
	mut a := [0].repeat(0)
	for i in 0 .. 10000 {
		a << i
	}
	assert a.len == 10000
	assert a[234] == 234
}

struct Chunk {
	val string
}

struct Kkk {
	q []Chunk
}

fn test_empty() {
	mut chunks := []Chunk{}
	a := Chunk{}
	assert chunks.len == 0
	chunks << a
	assert chunks.len == 1
	chunks = []
	assert chunks.len == 0
	chunks << a
	assert chunks.len == 1
}

fn test_push() {
	mut a := []int{}
	a << 1
	a << 3
	assert a[1] == 3
	assert a.str() == '[1, 3]'
}

fn test_insert() {
	mut a := [1, 2]
	a.insert(0, 3)
	assert a[0] == 3
	assert a[2] == 2
	assert a.len == 3
	a.insert(1, 4)
	assert a[1] == 4
	assert a[2] == 1
	assert a.len == 4
	a.insert(4, 5)
	assert a[4] == 5
	assert a[3] == 2
	assert a.len == 5
	mut b := []f64{}
	assert b.len == 0
	b.insert(0, f64(1.1))
	assert b.len == 1
	assert b[0] == f64(1.1)
}

fn test_insert_many() {
	mut a := [3, 4]
	a.insert(0, [1, 2])
	assert a == [1, 2, 3, 4]
	b := [5, 6]
	a.insert(1, b)
	assert a == [1, 5, 6, 2, 3, 4]
}

fn test_prepend() {
	mut a := []int{}
	assert a.len == 0
	a.prepend(1)
	assert a.len == 1
	assert a[0] == 1
	mut b := []f64{}
	assert b.len == 0
	b.prepend(f64(1.1))
	assert b.len == 1
	assert b[0] == f64(1.1)
}

fn test_prepend_many() {
	mut a := [3, 4]
	a.prepend([1, 2])
	assert a == [1, 2, 3, 4]
	b := [5, 6]
	a.prepend(b)
	assert a == [5, 6, 1, 2, 3, 4]
}

fn test_strings() {
	a := ['a', 'b', 'c']
	assert a.str() == "['a', 'b', 'c']"
}

/*
fn test_compare_ints() {
    assert compare_ints(1, 2) == -1
    assert compare_ints(2, 1) == 1
    assert compare_ints(0, 0) == 0

    a := 1
    b := 2
    assert compare_ints(a, b) == -1
    assert compare_ints(b, a) == 1
    assert compare_ints(a, a) == 0
}
*/
fn test_repeat() {
	{
		a := [0].repeat(5)
		assert a.len == 5
		assert a[0] == 0 && a[1] == 0 && a[2] == 0 && a[3] == 0 && a[4] == 0
	}
	{
		a := [1.1].repeat(10)
		assert a[0] == 1.1
		assert a[5] == 1.1
		assert a[9] == 1.1
	}
	{
		a := [i64(-123)].repeat(10)
		assert a[0] == -123
		assert a[5] == -123
		assert a[9] == -123
	}
	{
		a := [u64(123)].repeat(10)
		assert a[0] == 123
		assert a[5] == 123
		assert a[9] == 123
	}
	{
		a := [1.1].repeat(10)
		assert a[0] == 1.1
		assert a[5] == 1.1
		assert a[9] == 1.1
	}
	{
		a := [1, 2].repeat(2)
		assert a[0] == 1
		assert a[1] == 2
		assert a[2] == 1
		assert a[3] == 2
	}
	{
		a := ['1', 'abc'].repeat(2)
		assert a[0] == '1'
		assert a[1] == 'abc'
		assert a[2] == '1'
		assert a[3] == 'abc'
	}
	{
		mut a := ['1', 'abc'].repeat(0)
		assert a.len == 0
		a << 'abc'
		assert a[0] == 'abc'
	}
}

fn test_deep_repeat() {
	mut a3 := [[[1, 1], [2, 2], [3, 3]], [[4, 4], [5, 5], [6, 6]]]
	r := a3.repeat(3)
	a3[1][1][0] = 17
	assert r == [
		[[1, 1], [2, 2], [3, 3]],
		[[4, 4], [5, 5], [6, 6]],
		[[1, 1], [2, 2], [3, 3]],
		[[4, 4], [5, 5], [6, 6]],
		[[1, 1], [2, 2], [3, 3]],
		[[4, 4], [5, 5], [6, 6]],
	]
	assert a3 == [[[1, 1], [2, 2], [3, 3]], [[4, 4], [17, 5], [6, 6]]]
}

fn test_right() {
	a := [1, 2, 3, 4]
	c := a[1..a.len]
	d := a[1..]
	assert c[0] == 2
	assert c[1] == 3
	assert d[0] == 2
	assert d[1] == 3
}

fn test_left() {
	a := [1, 2, 3]
	c := a[0..2]
	d := a[..2]
	assert c[0] == 1
	assert c[1] == 2
	assert d[0] == 1
	assert d[1] == 2
}

fn test_slice() {
	a := [1, 2, 3, 4]
	b := a[2..4]
	assert b.len == 2
	assert a[1..2].len == 1
	assert a.len == 4
}

fn test_push_many() {
	mut a := [1, 2, 3]
	b := [4, 5, 6]
	a << b
	assert a.len == 6
	assert a[0] == 1
	assert a[3] == 4
	assert a[5] == 6
}

fn test_reverse() {
	a := [1, 2, 3, 4]
	b := ['test', 'array', 'reverse']
	c := a.reverse()
	println(c)
	d := b.reverse()
	for i, _ in c {
		assert c[i] == a[a.len - i - 1]
	}
	for i, _ in d {
		assert d[i] == b[b.len - i - 1]
	}
	e := []int{}
	f := e.reverse()
	assert f.len == 0
}

const (
	c_n = 5
)

struct Foooj {
	a [5]int // c_n
}

fn test_fixed() {
	mut nums := [4]int{}
	// x := nums[1..3]
	// assert x.len == 2
	assert nums[0] == 0
	assert nums[1] == 0
	assert nums[2] == 0
	assert nums[3] == 0
	nums[1] = 7
	assert nums[1] == 7
	nums2 := [5]int{} // c_n
	assert nums2[c_n - 1] == 0
}

fn modify(mut numbers []int) {
	numbers[0] = 777
}

fn test_mut_slice() {
	mut n := [1, 2, 3]
	// modify(mut n)
	modify(mut n[..2])
	assert n[0] == 777
	modify(mut n[2..])
	assert n[2] == 777
	println(n)
}

fn double_up(mut a []int) {
	for i := 0; i < a.len; i++ {
		a[i] = a[i] * 2
	}
}

fn double_up_v2(mut a []int) {
	for i, _ in a {
		a[i] = a[i] * 2 // or val*2, doesn't matter
	}
}

fn test_mut_arg() {
	mut arr := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
	double_up(mut arr)
	assert arr.str() == '[2, 4, 6, 8, 10, 12, 14, 16, 18, 20]'
	arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
	double_up_v2(mut arr)
	assert arr.str() == '[2, 4, 6, 8, 10, 12, 14, 16, 18, 20]'
}

fn test_clone() {
	nums := [1, 2, 3, 4, 100]
	_ = nums
	nums2 := nums.clone()
	assert nums2.len == 5
	assert nums.str() == '[1, 2, 3, 4, 100]'
	assert nums2.str() == '[1, 2, 3, 4, 100]'
	assert nums[1..3].str() == '[2, 3]'
}

/*
fn test_copy() {
	a := [1, 2, 3]
	b := a
	assert b[0] == 1
	assert b[1] == 2
	assert b[2] == 3
}
*/
fn test_multi_array_clone() {
	// 2d array_int
	mut a2_1 := [[1, 2, 3], [4, 5, 6]]
	mut a2_2 := a2_1.clone()
	a2_1[0][1] = 0
	a2_2[1][0] = 0
	assert a2_1 == [[1, 0, 3], [4, 5, 6]]
	assert a2_2 == [[1, 2, 3], [0, 5, 6]]
	// 2d array_string
	mut b2_1 := [['1', '2', '3'], ['4', '5', '6']]
	mut b2_2 := b2_1.clone()
	b2_1[0][1] = '0'
	b2_2[1][0] = '0'
	assert b2_1 == [['1', '0', '3'], ['4', '5', '6']]
	assert b2_2 == [['1', '2', '3'], ['0', '5', '6']]
	// 3d array_int
	mut a3_1 := [[[1, 1], [2, 2], [3, 3]], [[4, 4], [5, 5], [6, 6]]]
	mut a3_2 := a3_1.clone()
	a3_1[0][0][1] = 0
	a3_2[0][1][0] = 0
	assert a3_1 == [[[1, 0], [2, 2], [3, 3]], [[4, 4], [5, 5],
		[6, 6]]]
	assert a3_2 == [[[1, 1], [0, 2], [3, 3]], [[4, 4], [5, 5],
		[6, 6]]]
	// 3d array_string
	mut b3_1 := [[['1', '1'], ['2', '2'], ['3', '3']], [['4', '4'],
		['5', '5'], ['6', '6']]]
	mut b3_2 := b3_1.clone()
	b3_1[0][0][1] = '0'
	b3_2[0][1][0] = '0'
	assert b3_1 == [[['1', '0'], ['2', '2'], ['3', '3']], [['4', '4'],
		['5', '5'], ['6', '6']]]
	assert b3_2 == [[['1', '1'], ['0', '2'], ['3', '3']], [['4', '4'],
		['5', '5'], ['6', '6']]]
}

fn test_doubling() {
	mut nums := [1, 2, 3, 4, 5]
	for i in 0 .. nums.len {
		nums[i] *= 2
	}
	println(nums.str())
	assert nums.str() == '[2, 4, 6, 8, 10]'
}

struct Test2 {
	one int
	two int
}

struct Test {
	a string
mut:
	b []Test2
}

// TODO: default array/struct str methods
fn (ta []Test2) str() string {
	mut s := '['
	for i, t in ta {
		s += t.str()
		if i < ta.len - 1 {
			s += ', '
		}
	}
	s += ']'
	return s
}

fn (t Test2) str() string {
	return '{$t.one $t.two}'
}

fn (t Test) str() string {
	return '{$t.a $t.b}'
}

fn test_struct_print() {
	mut a := Test{
		a: 'Test'
		b: []
	}
	b := Test2{
		one: 1
		two: 2
	}
	a.b << b
	a.b << b
	assert a.str() == '{Test [{1 2}, {1 2}]}'
	assert b.str() == '{1 2}'
	assert a.b.str() == '[{1 2}, {1 2}]'
}

fn test_single_element() {
	mut a := [1]
	a << 2
	assert a.len == 2
	assert a[0] == 1
	assert a[1] == 2
	println(a)
}

fn test_find_index() {
	// string
	a := ['v', 'is', 'great']
	assert a.index('v') == 0
	assert a.index('is') == 1
	assert a.index('gre') == -1
	// int
	b := [1, 2, 3, 4]
	assert b.index(1) == 0
	assert b.index(4) == 3
	assert b.index(5) == -1
	// byte
	c := [0x22, 0x33, 0x55]
	assert c.index(0x22) == 0
	assert c.index(0x55) == 2
	assert c.index(0x99) == -1
	// char
	d := [`a`, `b`, `c`]
	assert d.index(`b`) == 1
	assert d.index(`c`) == 2
	assert d.index(`u`) == -1
}

fn test_multi() {
	a := [[1, 2, 3], [4, 5, 6]]
	assert a.len == 2
	assert a[0].len == 3
	assert a[0][0] == 1
	assert a[0][2] == 3
	assert a[1][2] == 6
	// TODO
	// b :=  [ [[1,2,3],[4,5,6]], [[1,2]] ]
	// assert b[0][0][0] == 1
}

fn test_in() {
	a := [1, 2, 3]
	assert 1 in a
	assert 2 in a
	assert 3 in a
	assert 4 !in a
	assert 0 !in a
	assert 0 !in a
	assert 4 !in a
	b := [1, 4, 0]
	c := [3, 6, 2, 0]
	assert 0 in b
	assert 0 in c
}

fn sum(prev int, curr int) int {
	return prev + curr
}

fn sub(prev int, curr int) int {
	return prev - curr
}

fn test_reduce() {
	a := [1, 2, 3, 4, 5]
	b := a.reduce(sum, 0)
	c := a.reduce(sum, 5)
	d := a.reduce(sum, -1)
	assert b == 15
	assert c == 20
	assert d == 14
	e := [1, 2, 3]
	f := e.reduce(sub, 0)
	g := e.reduce(sub, -1)
	assert f == -6
	assert g == -7
}

fn filter_test_helper_1(a int) bool {
	return a > 3
}

fn test_filter() {
	a := [1, 2, 3, 4, 5, 6]
	b := a.filter(it % 2 == 0)
	assert b.len == 3
	assert b[0] == 2
	assert b[1] == 4
	assert b[2] == 6
	c := ['v', 'is', 'awesome']
	d := c.filter(it.len > 1)
	assert d[0] == 'is'
	assert d[1] == 'awesome'
	////////
	arr := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
	println(arr.filter(it % 2 == 0 || it % 3 == 0))
	assert true
	assert [1, 2, 3].len == 3
	mut mut_arr := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
	mut_arr = mut_arr.filter(it < 4)
	assert mut_arr.len == 3
	assert a.filter(filter_test_helper_1) == [4, 5, 6]
	assert [1, 5, 10].filter(filter_test_helper_1) == [5, 10]
	// TODO
	// assert arr.filter(arr % 2).len == 5
}

fn test_anon_fn_filter() {
	filter_num := fn (i int) bool {
		return i % 2 == 0
	}
	assert [1, 2, 3, 4, 5].filter(filter_num) == [2, 4]
}

fn test_anon_fn_arg_filter() {
	a := [1, 2, 3, 4].filter(fn (i int) bool {
		return i % 2 == 0
	})
	assert a == [2, 4]
}

fn map_test_helper_1(i int) int {
	return i * i
}

fn map_test_helper_2(i int, b string) int {
	return i + b.len
}

fn map_test_helper_3(i int, b []string) int {
	return i + b.map(it.len)[i % b.len]
}

fn test_map() {
	nums := [1, 2, 3, 4, 5, 6]
	strs := ['v', 'is', 'awesome']
	// assert nums.map() == <error>
	// assert nums.map(it, 'excessive') == <error>
	// identity
	assert nums.map(it) == [1, 2, 3, 4, 5, 6]
	assert strs.map(it) == ['v', 'is', 'awesome']
	assert nums.map(it - it) == [0, 0, 0, 0, 0, 0]
	assert nums.map(it - it)[0] == 0
	// type switch
	assert nums.map(it * 10) == [10, 20, 30, 40, 50, 60]
	assert nums.map(it * it) == [1, 4, 9, 16, 25, 36]
	assert nums.map('$it') == ['1', '2', '3', '4', '5', '6']
	assert nums.map(it % 2 == 0) == [false, true, false, true, false, true]
	assert strs.map(it.to_upper()) == ['V', 'IS', 'AWESOME']
	assert strs.map(it == 'awesome') == [false, false, true]
	assert strs.map(it.len in nums) == [true, true, false]
	assert strs.map(int(7)) == [7, 7, 7]
	// external func
	assert nums.map(map_test_helper_1(it)) == [1, 4, 9, 16, 25, 36]
	assert nums.map(map_test_helper_2(it, 'bb')) == [3, 4, 5, 6, 7, 8]
	assert nums.map(map_test_helper_3(it, strs)) == [3, 9, 4, 6, 12, 7]
	// empty array as input
	assert []int{len: 0}.map(it * 2) == []
	// nested maps (where it is of same type)
	assert nums.map(strs.map(int(7)) == [7, 7, 7]) == [true, true, true, true, true, true]
	assert nums.map('$it' + strs.map('a')[0]) == ['1a', '2a', '3a', '4a', '5a', '6a']
	assert nums.map(it + strs.map(int(7))[0]) == [8, 9, 10, 11, 12, 13]
	assert nums.map(it + strs.map(it.len)[0]) == [2, 3, 4, 5, 6, 7]
	assert strs.map(it.len + strs.map(it.len)[0]) == [2, 3, 8]
	// nested (different it types)
	assert strs.map(it[nums.map(it - it)[0]]) == [u8(`v`), `i`, `a`]
	assert nums[0..3].map('$it' + strs.map(it)[it - 1]) == ['1v', '2is', '3awesome']
	assert nums.map(map_test_helper_1) == [1, 4, 9, 16, 25, 36]
	assert [1, 5, 10].map(map_test_helper_1) == [1, 25, 100]
	assert nums == [1, 2, 3, 4, 5, 6]
	assert strs == ['v', 'is', 'awesome']
}

fn test_anon_fn_map() {
	add_num := fn (i int) int {
		return i + 1
	}
	assert [1, 2, 3].map(add_num) == [2, 3, 4]
}

fn test_multi_anon_fn_map() {
	a := [1, 2, 3].map(fn (i int) int {
		return i + 1
	})
	b := [1, 2, 3].map(fn (i int) int {
		return i + 2
	})
	assert a == [2, 3, 4]
	assert b == [3, 4, 5]
}

fn test_anon_fn_arg_map() {
	a := [1, 2, 3].map(fn (i int) int {
		return i + 1
	})
	assert a == [2, 3, 4]
}

fn test_anon_fn_arg_different_type_map() {
	i_to_str := fn (i int) string {
		return i.str()
	}
	a := [1, 2, 3].map(i_to_str)
	assert a == ['1', '2', '3']
}

fn test_anon_fn_inline_different_type_map() {
	a := [1, 2, 3].map(fn (i int) string {
		return i.str()
	})
	assert a == ['1', '2', '3']
}

fn test_array_str() {
	numbers := [1, 2, 3]
	assert numbers == [1, 2, 3]
	numbers2 := [numbers, [4, 5, 6]] // dup str() bug
	println(numbers2)
	assert true
	assert numbers.str() == '[1, 2, 3]'
	// QTODO
	// assert numbers2.str() == '[[1, 2, 3], [4, 5, 6]]'
}

struct User {
	age  int
	name string
}

fn test_eq() {
	assert [5, 6, 7] != [6, 7]
	assert [`a`, `b`] == [`a`, `b`]
	assert [User{
		age: 22
		name: 'bob'
	}] == [User{
		age: 22
		name: 'bob'
	}]
	assert [{
		'bob': 22
	}, {
		'tom': 33
	}] == [{
		'bob': 22
	}, {
		'tom': 33
	}]
	assert [[1, 2, 3], [4]] == [[1, 2, 3], [4]]
}

fn test_fixed_array_eq() {
	a1 := [1, 2, 3]!
	assert a1 == [1, 2, 3]!
	assert a1 != [2, 3, 4]!

	a2 := [[1, 2]!, [3, 4]!]!
	assert a2 == [[1, 2]!, [3, 4]!]!
	assert a2 != [[3, 4]!, [1, 2]!]!

	a3 := [[1, 2], [3, 4]]!
	assert a3 == [[1, 2], [3, 4]]!
	assert a3 != [[1, 1], [2, 2]]!

	a4 := [[`a`, `b`], [`c`, `d`]]!
	assert a4 == [[`a`, `b`], [`c`, `d`]]!
	assert a4 != [[`c`, `a`], [`a`, `b`]]!

	a5 := [['aaa', 'bbb'], ['ccc', 'ddd']]!
	assert a5 == [['aaa', 'bbb'], ['ccc', 'ddd']]!
	assert a5 != [['abc', 'def'], ['ccc', 'ddd']]!

	a6 := [['aaa', 'bbb']!, ['ccc', 'ddd']!]!
	assert a6 == [['aaa', 'bbb']!, ['ccc', 'ddd']!]!
	assert a6 != [['aaa', 'bbb']!, ['aaa', 'ddd']!]!

	a7 := [[1, 2]!, [3, 4]!]
	assert a7 == [[1, 2]!, [3, 4]!]
	assert a7 != [[2, 3]!, [1, 2]!]

	a8 := [['aaa', 'bbb']!, ['ccc', 'ddd']!]
	assert a8 == [['aaa', 'bbb']!, ['ccc', 'ddd']!]
	assert a8 != [['bbb', 'aaa']!, ['cccc', 'dddd']!]
}

fn test_fixed_array_literal_eq() {
	assert [1, 2, 3]! == [1, 2, 3]!
	assert [1, 1, 1]! != [1, 2, 3]!

	assert [[1, 2], [3, 4]]! == [[1, 2], [3, 4]]!
	assert [[1, 1], [2, 2]]! != [[1, 2], [3, 4]]!

	assert [[1, 1]!, [2, 2]!]! == [[1, 1]!, [2, 2]!]!
	assert [[1, 1]!, [2, 2]!]! != [[1, 2]!, [2, 3]!]!

	assert [[1, 1]!, [2, 2]!] == [[1, 1]!, [2, 2]!]
	assert [[1, 1]!, [2, 2]!] != [[1, 2]!, [2, 3]!]
}

fn test_sort() {
	mut a := ['hi', '1', '5', '3']
	a.sort()
	assert a == ['1', '3', '5', 'hi']

	mut nums := [67, -3, 108, 42, 7]
	nums.sort()
	assert nums == [-3, 7, 42, 67, 108]

	nums.sort(a < b)
	assert nums == [-3, 7, 42, 67, 108]

	nums.sort(b < a)
	assert nums == [108, 67, 42, 7, -3]

	mut users := [User{22, 'Peter'}, User{20, 'Bob'}, User{25, 'Alice'}]
	users.sort(a.age < b.age)
	assert users[0].age == 20
	assert users[1].age == 22
	assert users[2].age == 25
	assert users[0].name == 'Bob'
	assert users[1].name == 'Peter'
	assert users[2].name == 'Alice'

	users.sort(a.age > b.age)
	assert users[0].age == 25
	assert users[1].age == 22
	assert users[2].age == 20

	users.sort(b.age > a.age)
	assert users[0].age == 20
	assert users[1].age == 22
	assert users[2].age == 25

	users.sort(a.name < b.name)
	assert users[0].name == 'Alice'
	assert users[1].name == 'Bob'
	assert users[2].name == 'Peter'
}

fn test_sort_with_compare() {
	mut a := ['hi', '1', '5', '3']
	a.sort_with_compare(fn (a &string, b &string) int {
		if a < b {
			return -1
		}
		if a > b {
			return 1
		}
		return 0
	})
	assert a == ['1', '3', '5', 'hi']
}

fn test_rune_sort() {
	mut bs := [`f`, `e`, `d`, `b`, `c`, `a`]
	bs.sort()
	println(bs)
	assert bs == [`a`, `b`, `c`, `d`, `e`, `f`]

	bs.sort(a > b)
	println(bs)
	assert bs == [`f`, `e`, `d`, `c`, `b`, `a`]

	bs.sort(a < b)
	println(bs)
	assert bs == [`a`, `b`, `c`, `d`, `e`, `f`]
}

fn test_sort_by_different_order_of_a_b() {
	mut x := [1, 2, 3]
	x.sort(a < b)
	println(x)
	assert x == [1, 2, 3]

	mut y := [1, 2, 3]
	y.sort(b < a)
	println(y)
	assert y == [3, 2, 1]
}

fn test_f32_sort() {
	mut f := [f32(50.0), 15, 1, 79, 38, 0, 27]
	f.sort()
	assert f == [f32(0.0), 1, 15, 27, 38, 50, 79]

	f.sort(a < b)
	assert f == [f32(0.0), 1, 15, 27, 38, 50, 79]

	f.sort(b > a)
	assert f == [f32(0.0), 1, 15, 27, 38, 50, 79]

	f.sort(b < a)
	assert f == [f32(79.0), 50, 38, 27, 15, 1, 0]

	f.sort(a > b)
	assert f == [f32(79.0), 50, 38, 27, 15, 1, 0]
}

fn test_f64_sort() {
	mut f := [50.0, 15, 1, 79, 38, 0, 27]
	f.sort()
	assert f[0] == 0.0
	assert f[1] == 1.0
	assert f[6] == 79.0
}

fn test_i64_sort() {
	mut f := [i64(50), 15, 1, 79, 38, 0, 27]
	f.sort()
	assert f[0] == 0
	assert f[1] == 1
	assert f[6] == 79
}

fn test_sort_index_expr() {
	mut f := [[i64(50), 48], [i64(15)], [i64(1)], [i64(79)], [i64(38)],
		[i64(0)], [i64(27)]]
	// TODO This currently gives "indexing pointer" error without unsafe
	unsafe {
		f.sort(a[0] < b[0])
	}
	assert f == [[i64(0)], [i64(1)], [i64(15)], [i64(27)], [i64(38)],
		[i64(50), 48], [i64(79)]]
}

fn test_a_b_paras_sort() {
	mut arr_i := [1, 3, 2]
	arr_i.sort(a < b)
	println(arr_i)
	assert arr_i == [1, 2, 3]
	arr_i.sort(b < a)
	println(arr_i)
	assert arr_i == [3, 2, 1]

	mut arr_f := [1.1, 3.3, 2.2]
	arr_f.sort(a < b)
	println(arr_f)
	assert arr_f == [1.1, 2.2, 3.3]
	arr_f.sort(b < a)
	println(arr_f)
	assert arr_f == [3.3, 2.2, 1.1]
}

/*
fn test_for_last() {
	numbers := [1, 2, 3, 4]
	mut s := '['
	for num in numbers {
		s += '$num'
		if !last {
			s += ', '

		}
	}
	s += ']'
	assert s == '[1, 2, 3, 4]'
}
*/
struct Foo {
mut:
	bar []int
}

fn test_in_struct() {
	mut baz := Foo{
		bar: [0, 0, 0]
	}
	baz.bar[0] += 2
	baz.bar[0]++
	assert baz.bar[0] == 3
}

[direct_array_access]
fn test_direct_modification() {
	mut foo := [2, 0, 5]
	foo[1] = 3
	foo[0] *= 7
	foo[1]--
	foo[2] -= 2
	assert foo[0] == 14
	assert foo[1] == 2
	assert foo[2] == 3
}

fn test_bools() {
	println('test b')
	mut a := [true, false]
	a << true
	println(a)
}

fn test_push_many_self() {
	mut actual_arr := [1, 2, 3, 4]
	actual_arr << actual_arr
	expected_arr := [1, 2, 3, 4, 1, 2, 3, 4]
	assert actual_arr.len == expected_arr.len
	for i in 0 .. actual_arr.len {
		assert actual_arr[i] == expected_arr[i]
	}
}

fn test_for() {
	nums := [1, 2, 3]
	mut sum := 0
	for num in nums {
		sum += num
	}
	assert sum == 6
}

fn test_clear() {
	mut arr := [1, 2, 3]
	assert arr.len == 3
	arr.clear()
	assert arr.len == 0
	arr << 3
	arr << 2
	arr << 1
	arr << 0
	assert arr.len == 4
	assert arr[0] == 3
	assert arr[1] == 2
	assert arr[2] == 1
	assert arr[3] == 0
	arr.clear()
	assert arr.len == 0
}

fn test_trim() {
	mut arr := [1, 2, 3, 4, 5, 6, 7, 8, 9]
	assert arr.len == 9
	arr.trim(9)
	assert arr.len == 9
	assert arr.last() == 9
	arr.trim(7)
	assert arr.len == 7
	assert arr.last() == 7
	arr.trim(2)
	assert arr.len == 2
	assert arr.last() == 2
}

[manualfree]
fn test_drop() {
	mut a := [1, 2]
	a << 3 // pushing assures reallocation; a.cap now should be bigger:
	assert a.cap > 3
	// eprintln('>>> a.cap: $a.cap | a.len: $a.len')

	a.drop(-1000)
	assert a == [1, 2, 3] // a.drop( negative ) should NOT modify the array
	// eprintln('>>> a.cap: $a.cap | a.len: $a.len')

	a.drop(2)
	assert a == [3]
	assert a.cap > a.len
	// eprintln('>>> a.cap: $a.cap | a.len: $a.len')

	a.drop(10)
	assert a == []
	assert a.cap > a.len
	// eprintln('>>> a.cap: $a.cap | a.len: $a.len')

	a << 123
	a << 456
	a << 789
	// eprintln('>>> a.cap: $a.cap | a.len: $a.len')
	assert a == [123, 456, 789]

	a.drop(10)
	assert a == []
	// eprintln('>>> a.cap: $a.cap | a.len: $a.len')

	unsafe { a.free() } // test offset OK
}

fn test_hex() {
	// array hex
	st := [u8(`V`), `L`, `A`, `N`, `G`]
	assert st.hex() == '564c414e47'
	assert st.hex().len == 10
	st1 := [u8(0x41)].repeat(100)
	assert st1.hex() == '41'.repeat(100)
}

fn test_left_shift_precendence() {
	mut arr := []int{}
	arr << 1 + 1
	arr << 1 - 1
	arr << 2 / 1
	arr << 2 * 1
	assert arr[0] == 2
	assert arr[1] == 0
	assert arr[2] == 2
	assert arr[3] == 2
}

fn test_array_with_cap() {
	a4 := []int{len: 1, cap: 10}
	assert a4.len == 1
	assert a4.cap == 10
	a5 := []int{len: 1, cap: 10}
	assert a5.len == 1
	assert a5.cap == 10
}

fn test_multi_array_index() {
	mut a := [][]int{len: 2, init: []int{len: 3, init: 0}}
	a[0][0] = 1
	assert '$a' == '[[1, 0, 0], [0, 0, 0]]'
	mut b := [[0].repeat(3)].repeat(2)
	b[0][0] = 1
	assert '$b' == '[[1, 0, 0], [0, 0, 0]]'
}

fn test_plus_assign_string() {
	mut a := ['']
	a[0] += 'abc'
	assert a == ['abc']
}

fn mut_arr_with_eq_in_fn(mut a []int) {
	if a == [1, 2, 3, 4] {
		a[0] = 0
	}
	if [0, 2, 3, 4] == a {
		a[1] = 0
	}
	if !(a != [0, 0, 3, 4]) {
		a[2] = 0
	}
	if !([0, 0, 0, 4] != a) {
		a[3] = 0
	}
}

fn test_mut_arr_with_eq_in_fn() {
	mut a := [1, 2, 3, 4]
	mut_arr_with_eq_in_fn(mut a)
	assert a == [0, 0, 0, 0]
}

fn array_in_mut(mut a []int) {
	if 1 in a {
		a[0] = 2
	}
}

fn test_array_in_mut() {
	mut a := [1, 2]
	array_in_mut(mut a)
	assert a == [2, 2]
}

// test array delete in function with mut argument
fn delete_nums(mut arr []int) {
	arr.delete(0)
}

fn test_array_delete_in_mut() {
	mut nums := [1, 2, 3]
	delete_nums(mut nums)
	assert nums == [2, 3]
}

// test array add in function with mut argument
fn add_nums(mut arr []int) {
	arr << 4
}

fn test_array_add_in_mut() {
	mut nums := [1, 2, 3]
	add_nums(mut nums)
	assert nums == [1, 2, 3, 4]
}

fn test_reverse_in_place() {
	mut a := [1, 2, 3, 4]
	a.reverse_in_place()
	assert a == [4, 3, 2, 1]
	mut b := ['a', 'b', 'c']
	b.reverse_in_place()
	assert b == ['c', 'b', 'a']
	mut c := [[1, 2], [3, 4], [5, 6]]
	c.reverse_in_place()
	assert c == [[5, 6], [3, 4], [1, 2]]
}

fn test_array_int_pop() {
	mut a := [1, 2, 3, 4, 5]
	assert a.len == 5
	x := a.last()
	y := a.pop()
	assert x == y
	assert a.len == 4
	z := a.pop()
	assert a.len == 3
	assert z == 4
	x1 := a.pop()
	x2 := a.pop()
	final := a.pop()
	assert final == 1
}

fn test_array_string_pop() {
	mut a := ['abc', 'def', 'xyz']
	assert a.len == 3
	assert a.pop() == 'xyz'
	assert a.pop() == 'def'
	assert a.pop() == 'abc'
	assert a.len == 0
	assert a.cap == 3
}

fn test_array_first() {
	a := [3]
	assert a.first() == 3
	b := [1, 2, 3, 4]
	assert b.first() == 1
	c := ['abc', 'def']
	assert c.first()[0] == `a`
	s := [Chunk{'a'}]
	assert s.first().val == 'a'
}

fn test_array_last() {
	a := [3]
	assert a.last() == 3
	b := [1, 2, 3, 4]
	assert b.last() == 4
	c := ['abc', 'def']
	assert c.last()[0] == `d`
	s := [Chunk{'a'}]
	assert s.last().val == 'a'
}

[direct_array_access]
fn test_direct_array_access() {
	mut a := [11, 22, 33, 44]
	assert a[0] == 11
	assert a[2] == 33
	x := a[0]
	a[0] = 21
	a[1] += 2
	a[2] = x + 3
	a[3] -= a[1]
	assert a == [21, 24, 14, 20]
}

[direct_array_access]
fn test_direct_array_access_via_ptr() {
	mut b := [11, 22, 33, 44]
	unsafe {
		mut a := &b
		assert a[0] == 11
		assert a[2] == 33
		x := a[0]
		a[0] = 21
		a[1] += 2
		a[2] = x + 3
		a[3] -= a[1]
		assert a == [21, 24, 14, 20]
	}
}

fn test_push_arr_string_free() {
	mut lines := ['hi']
	s := 'a' + 'b'
	lines << s
	// make sure the data in the array is valid after freeing the string
	unsafe { s.free() }
	//
	println(lines)
	assert lines.len == 2
	assert lines[0] == 'hi'
	assert lines[1] == 'ab'
}

const (
	grid_size_1 = 2
	grid_size_2 = 3
	grid_size_3 = 4
	cell_value  = 123
)

fn test_multidimensional_array_initialization_with_consts() {
	mut data := [][][]int{len: grid_size_1, init: [][]int{len: grid_size_2, init: []int{len: grid_size_3, init: cell_value}}}
	assert data.len == grid_size_1
	assert data[0].len == grid_size_2
	assert data[0][0].len == grid_size_3
	assert data[0][0][0] == cell_value
	assert data[1][1][1] == cell_value
}

fn test_byteptr_vbytes() {
	unsafe {
		bp := malloc(5)
		bp[0] = 1
		bp[1] = 2
		bp[2] = 3
		bp[3] = 4
		bp[4] = 255
		bytes := bp.vbytes(5)
		println(bytes)
		assert bytes.len == 5
		assert bytes[0] == 1
		assert bytes[1] == 2
		assert bytes[2] == 3
		assert bytes[3] == 4
		assert bytes[4] == 255
	}
}

fn test_voidptr_vbytes() {
	unsafe {
		bp := malloc(3)
		bp[0] = 4
		bp[1] = 5
		bp[2] = 6
		bytes := voidptr(bp).vbytes(3)
		assert bytes.len == 3
		assert bytes[0] == 4
		assert bytes[1] == 5
		assert bytes[2] == 6
		println(bytes)
	}
}

fn test_multi_array_prepend() {
	mut a := [][]int{}
	a.prepend([1, 2, 3])
	assert a == [[1, 2, 3]]
	mut b := [][]int{}
	b.prepend([[1, 2, 3]])
	assert b == [[1, 2, 3]]
}

fn test_multi_array_insert() {
	mut a := [][]int{}
	a.insert(0, [1, 2, 3])
	assert a == [[1, 2, 3]]
	mut b := [][]int{}
	b.insert(0, [[1, 2, 3]])
	assert b == [[1, 2, 3]]
}

fn test_multi_array_in() {
	a := [[1]]
	println([1] in a)
	assert [1] in a
}

fn test_any_type_array_contains() {
	a := [true, false]
	assert a.contains(true)
	assert true in a
	assert a.contains(false)
	assert false in a
	b := [i64(2), 3, 4]
	assert b.contains(i64(3))
	assert 5 !in b
	c := [[1], [2]]
	assert c.contains([1])
	assert [2] in c
	assert [3] !in c
}

struct Person {
	name string
	nums []int
	kv   map[string]string
}

fn test_struct_array_of_multi_type_in() {
	ivan := Person{
		name: 'ivan'
		nums: [1, 2, 3]
		kv: {
			'aaa': '111'
		}
	}
	people := [
		Person{
			name: 'ivan'
			nums: [1, 2, 3]
			kv: {
				'aaa': '111'
			}
		},
		Person{
			name: 'bob'
			nums: [2]
			kv: {
				'bbb': '222'
			}
		},
	]
	println(ivan in people)
	assert ivan in people
}

fn test_struct_array_of_multi_type_index() {
	ivan := Person{
		name: 'ivan'
		nums: [1, 2, 3]
		kv: {
			'aaa': '111'
		}
	}
	people := [
		Person{
			name: 'ivan'
			nums: [1, 2, 3]
			kv: {
				'aaa': '111'
			}
		},
		Person{
			name: 'bob'
			nums: [2]
			kv: {
				'bbb': '222'
			}
		},
	]
	println(people.index(ivan))
	assert people.index(ivan) == 0
}

struct Coord {
	x int
	y int
	z int
}

fn test_array_struct_contains() {
	mut coords := []Coord{}
	coord_1 := Coord{
		x: 1
		y: 2
		z: -1
	}
	coords << coord_1
	exists := coord_1 in coords
	not_exists := coord_1 !in coords
	println('`exists`: $exists and `not exists`: $not_exists')
	assert exists == true
	assert not_exists == false
}

fn test_array_struct_ref_contains() {
	mut coords := []&Coord{}
	coord_1 := &Coord{
		x: 1
		y: 2
		z: -1
	}
	coords << coord_1
	exists := coord_1 in coords
	println(exists)
	assert exists == true
}

fn test_array_struct_ref_index() {
	mut coords := []&Coord{}
	coord_1 := &Coord{
		x: 1
		y: 2
		z: -1
	}
	coords << coord_1
	println(coords.index(coord_1))
	assert coords.index(coord_1) == 0
}

fn test_array_of_array_append() {
	mut x := [][]int{len: 4}
	println(x) // OK
	x[2] << 123 // RTE
	println(x)
	assert '$x' == '[[], [], [123], []]'
}

fn test_array_of_map_insert() {
	mut x := []map[string]int{len: 4}
	println(x) // OK
	x[2]['123'] = 123 // RTE
	println(x)
	assert '$x' == "[{}, {}, {'123': 123}, {}]"
}

fn test_multi_fixed_array_init() {
	a := [3][3]int{}
	assert '$a' == '[[0, 0, 0], [0, 0, 0], [0, 0, 0]]'
}

struct Numbers {
	odds  []int
	evens []int
}

fn test_array_of_multi_filter() {
	arr := [1, 2, 3, 4, 5]
	nums := Numbers{
		odds: arr.filter(it % 2 == 1)
		evens: arr.filter(it % 2 == 0)
	}
	println(nums)
	assert nums.odds == [1, 3, 5]
	assert nums.evens == [2, 4]
}

fn test_array_of_multi_map() {
	arr := [1, 3, 5]
	nums := Numbers{
		odds: arr.map(it + 2)
		evens: arr.map(it * 2)
	}
	println(nums)
	assert nums.odds == [3, 5, 7]
	assert nums.evens == [2, 6, 10]
}

fn test_multi_fixed_array_with_default_init() {
	a := [3][3]int{init: [3]int{init: 10}}
	println(a)
	assert a == [[10, 10, 10]!, [10, 10, 10]!, [10, 10, 10]!]!
}

struct Abc {
mut:
	x i64
	y i64
	z i64
}

fn test_clone_of_same_elem_size_array() {
	mut arr := []Abc{}
	arr << Abc{1, 2, 3}
	arr << Abc{2, 3, 4}
	arr2 := arr.clone()
	println(arr2)
	assert arr2 == [Abc{1, 2, 3}, Abc{2, 3, 4}]
}

pub fn example<T>(mut arr []T) []T {
	return arr.clone()
}

fn test_generic_mutable_arrays() {
	mut arr := [1, 2, 3]
	assert example(mut arr) == [1, 2, 3]
}
