const (
	q = [1, 2, 3]
	A = 8
)

fn test_pointer() {
	mut arr := []&int
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
	assert *d_arr[1][0] == 1
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
	for i in 0..10000 {
		a << i
	}
	assert a.len == 10000
	assert a[234] == 234
}

struct Chunk {
	val string
}

struct K {
	q []Chunk
}

fn test_empty() {
	mut chunks := []Chunk
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
	mut a := []int
	a << 1
	a << 3
	assert a[1] == 3
	assert a.str() == '[1, 3]'
}

// TODO array.insert is broken
// Cannot pass literal or primitive type as it cannot be cast to voidptr.
// In the current state only that would work:
// i := 3
// a.insert(0, &i)
// ----------------------------
/*
fn test_insert() {
	mut a := [1, 2]
	a.insert(0, 3)
	println(a)
}
*/
// fn test_insert() {
// mut a := [1, 2]
// a.insert(0, 3)
// assert a[0] == 3
// assert a[2] == 2
// assert a.len == 3
// a.insert(1, 4)
// assert a[1] == 4
// assert a[2] == 1
// assert a.len == 4
// a.insert(4, 5)
// assert a[4] == 5
// assert a[3] == 2
// assert a.len == 5
// mut b := []f64
// assert b.len == 0
// b.insert(0, f64(1.1))
// assert b.len == 1
// assert b[0] == f64(1.1)
// }
// TODO array.prepend is broken
// It depends on array.insert
// -----------------------------
// fn test_prepend() {
// mut a := []int
// assert a.len == 0
// a.prepend(1)
// assert a.len == 1
// assert a[0] == 1
// mut b := []f64
// assert b.len == 0
// b.prepend(f64(1.1))
// assert b.len == 1
// assert b[0] == f64(1.1)
// }
fn test_strings() {
	a := ['a', 'b', 'c']
	assert a.str() == '["a", "b", "c"]'
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
		a := [f64(1.1)].repeat(10)
		assert a[0] == f64(1.1)
		assert a[5] == f64(1.1)
		assert a[9] == f64(1.1)
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
	b := a.slice(2, 4)
	assert b.len == 2
	assert a.slice(1, 2).len == 1
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
	d := b.reverse()
	for i, _ in c {
		assert c[i] == a[a.len - i - 1]
	}
	for i, _ in d {
		assert d[i] == b[b.len - i - 1]
	}

	e := []int
	f := e.reverse()
	assert f.len == 0
}

const (
	N = 5
)

struct Foooj {
	a [N]int
}

fn test_fixed() {
	mut nums := [4]int
	assert nums[0] == 0
	assert nums[1] == 0
	assert nums[2] == 0
	assert nums[3] == 0
	nums[1] = 7
	assert nums[1] == 7
	nums2 := [N]int
	assert nums2[N - 1] == 0
}

fn modify(numbers mut []int) {
	numbers[0] = 777
}

fn test_mut_slice() {
	mut n := [1, 2, 3]
	modify(mut n[..2])
	assert n[0] == 777
	modify(mut n[2..])
	assert n[2] == 777
	println(n)
}

fn test_clone() {
	nums := [1, 2, 3, 4, 100]
	nums2 := nums.clone()
	assert nums2.len == 5
	assert nums2.str() == '[1, 2, 3, 4, 100]'
	assert nums.slice(1, 3).str() == '[2, 3]'
}

fn test_doubling() {
	mut nums := [1, 2, 3, 4, 5]
	for i in 0..nums.len {
		nums[i] *= 2
	}
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

pub fn (t Test2) str() string {
	return '{$t.one $t.two}'
}

pub fn (t Test) str() string {
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
	assert a.str() == '{Test [{1 2}, {1 2}] }'
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
	assert !(4 in a)
	assert !(0 in a)
}

fn sum(prev int, curr int) int {
	return prev + curr
}

fn sub(prev int, curr int) int {
	return prev - curr
}

/*
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
*/

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
}

fn test_map() {
	a := [1, 2, 3, 4, 5, 6]
	b := a.map(it * 10)
	assert b.len == 6
	assert b[0] == 10
	assert b[1] == 20
	assert b[2] == 30
	c := ['v', 'is', 'awesome']
	d := c.map(it.to_upper())
	assert d[0] == 'V'
	assert d[1] == 'IS'
	assert d[2] == 'AWESOME'
	bools := c.map(it == 'v')
	assert bools.len == 3
	assert bools[0] == true
	assert bools[1] == false
	assert bools[2] == false
}

fn test_array_str() {
	numbers := [1, 2, 3]
	// assert numbers == [1,2,3]
	numbers2 := [numbers, [4, 5, 6]] // dup str() bug
	assert true
	assert numbers.str() == '[1, 2, 3]'
	assert numbers2.str() == '[[1, 2, 3], [4, 5, 6]]'
}

fn test_eq() {
	/*
	assert [5, 6, 7].eq([6, 7]) == false
	assert [`a`, `b`].eq([`a`, `b`]) == true
	*/
}

fn test_sort() {
	mut a := ['hi', '1', '5', '3']
	a.sort()
	assert a[0] == '1'
	assert a[1] == '3'
	assert a[2] == '5'
	assert a[3] == 'hi'
	//
	mut nums := [67, -3, 108, 42, 7]
	nums.sort()
	assert nums[0] == -3
	assert nums[1] == 7
	assert nums[2] == 42
	assert nums[3] == 67
	assert nums[4] == 108
}

fn test_f32_sort() {
	mut f := [50.0, 15, 1, 79, 38, 0, 27]
	f.sort_with_compare(compare_f32)
	assert f[0] == 0.0
	assert f[1] == 1.0
	assert f[6] == 79.0
}

fn test_f64_sort() {
	mut f := [f64(50.0), 15, 1, 79, 38, 0, 27]
	f.sort_with_compare(compare_f64)
	assert f[0] == 0.0
	assert f[1] == 1.0
	assert f[6] == 79.0
}

fn test_i64_sort() {
	mut f := [i64(50), 15, 1, 79, 38, 0, 27]
	f.sort_with_compare(compare_i64)
	assert f[0] == 0
	assert f[1] == 1
	assert f[6] == 79
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
    for i in 0..actual_arr.len {
        assert actual_arr[i] == expected_arr[i]
    }
}

fn test_for() {
	nums := [1,2,3]
	mut sum := 0
	for num <- nums {
		sum += num
	}
	assert sum == 6
}

fn test_clear() {
	mut arr := [1,2,3]
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
	mut arr := [1,2,3,4,5,6,7,8,9]
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
