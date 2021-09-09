struct Chunk {
	val string
}

struct Kkk {
	q []Chunk
}

const (
	c_n = 5
)

struct Coord {
	x int
	y int
	z int
}

struct Numbers {
	odds  []int
	evens []int
}

struct Person {
	name string
	nums []int
	kv   map[string]string
}

// test array add in function with mut argument
fn add_nums(mut arr []int) {
	arr << 4
}

const (
	grid_size_1 = 2
	grid_size_2 = 3
	grid_size_3 = 4
	cell_value  = 123
)

struct User {
	age  int
	name string
}

struct Foo {
mut:
	bar []int
}

fn array_in_mut(mut a []int) {
	if 1 in a {
		a[0] = 2
	}
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

fn map_test_helper_1(i int) int {
	return i * i
}

fn map_test_helper_2(i int, b string) int {
	return i + b.len
}

fn map_test_helper_3(i int, b []string) int {
	return i + b.map(it.len)[i % b.len]
}

fn filter_test_helper_1(a int) bool {
	return a > 3
}

fn sum(prev int, curr int) int {
	return prev + curr
}

fn sub(prev int, curr int) int {
	return prev - curr
}

struct Foooj {
	a [5]int // c_n
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

fn modify(mut numbers []int) {
	numbers[0] = 777
}

fn main() {
	{
		// test pointer
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
		println(*d_arr[0][1]) // 3
		println(*d_arr[1][0]) // 1
	}
	{
		// test assign
		mut arr := [2, 4, 8, 16, 32, 64, 128]
		arr[0] = 2
		arr[1] &= 255
		arr[2] |= 255
		arr[3] <<= 4
		arr[4] >>= 4
		arr[5] %= 5
		arr[6] ^= 3
		println(arr[0])
		println(arr[1])
		println(arr[2])
		println(arr[3])
		println(arr[4])
		println(arr[5])
		println(arr[6])
	}
	{
		// test ints
		mut a := [1, 5, 2, 3]
		println(a.len) // 4
		println(a[0])
		println(a[2])
		println(a.last())

		a << 4
		println(a.len)
		println(a[4])
		println(a.last())

		s := a.str()
		println(s)
		println(a[1])
		println(a.last())
	}
	{
		// test deleting
		mut a := [1, 5, 2, 3, 4]

		println(a.len)
		println(a.str())

		a.delete(0)

		println(a.str())
		println(a.len) // 4

		a.delete(1)

		println(a.str())
		println(a.len)
		a.delete(a.len - 1)

		println(a.str())
		println(a.len)
	}
	{
		// test slice delete
		mut a := [1.5, 2.5, 3.25, 4.5, 5.75]
		b := a[2..4]
		a.delete(0)
		// assert a == [2.5, 3.25, 4.5, 5.75]
		// assert b == [3.25, 4.5]
		println(a)
		println(a == [2.5, 3.25, 4.5, 5.75])
		println(b == [3.25, 4.5])
		a = [3.75, 4.25, -1.5, 2.25, 6.0]
		c := a[..3]
		a.delete(2)
		println(a == [3.75, 4.25, 2.25, 6.0])
		println(c == [3.75, 4.25, -1.5])
	}
	{
		// test delete many
		mut a := [1, 2, 3, 4, 5, 6, 7, 8, 9]
		b := a[2..6]
		a.delete_many(4, 3)
		println(a == [1, 2, 3, 4, 8, 9])
		println(b == [3, 4, 5, 6])

		c := a[..a.len]
		a.delete_many(2, 0) // this should just clone
		a[1] = 17

		println(a == [1, 17, 3, 4, 8, 9])
		println(c == [1, 2, 3, 4, 8, 9])
		a.delete_many(0, a.len)
		println(a == []int{})
	}
	{
		// test short
		a := [1, 2, 3]
		println(a.len == 3)
		println(a.cap == 3)
		println(a[0])
		println(a[1])
		println(a[2])
	}
	{
		// test large
		mut a := [0].repeat(0)
		for i in 0 .. 10000 {
			a << i
		}
		println(a.len)
		println(a[234])
	}
	{
		// test empty
		mut chunks := []Chunk{}
		a := Chunk{}
		println(chunks.len)
		chunks << a
		println(chunks.len)
		chunks = []
		println(chunks.len)
		chunks << a
		println(chunks.len)
	}
	{
		// test push
		mut a := []int{}
		a << 1
		a << 3
		println(a[1])
		println(a.str())
	}
	{
		// test insert
		mut a := [1, 2]
		a.insert(0, 3)
		println(a[0])
		println(a[2])
		println(a.len)
		a.insert(1, 4)
		println(a[1])
		println(a[2])
		println(a.len)
		a.insert(4, 5)
		println(a[4])
		println(a[3])
		println(a.len)
		mut b := []f64{}
		println(b.len)
		b.insert(0, f64(1.1))
		println(b.len)
		println(b[0])
	}
	{
		// test insert many
		mut a := [3, 4]
		a.insert(0, [1, 2])
		println(a)

		b := [5, 6]
		a.insert(1, b)
		println(a)
	}
	{
		// test prepend
		mut a := []int{}
		println(a.len)
		a.prepend(1)
		println(a.len)
		println(a[0])
		mut b := []f64{}

		println(b.len)

		b.prepend(f64(1.1))

		println(b.len)

		println(b[0])
	}
	{
		// test prepend many
		mut a := [3, 4]
		a.prepend([1, 2])
		println(a)
		b := [5, 6]
		a.prepend(b)
		println(a)
	}
	{
		// test repeat
		{
			a := [0].repeat(5)
			println(a.len)
			println(a[0] == 0 && a[1] == 0 && a[2] == 0 && a[3] == 0 && a[4] == 0)
		}
		{
			a := [1.1].repeat(10)
			println(a[0])
			println(a[5])
			println(a[9])
		}
		{
			a := [i64(-123)].repeat(10)
			println(a[0])
			println(a[5])
			println(a[9])
		}
		{
			a := [u64(123)].repeat(10)
			println(a[0])
			println(a[5])
			println(a[9])
		}
		{
			a := [1.1].repeat(10)
			println(a[0])
			println(a[5])
			println(a[9])
		}
		{
			a := [1, 2].repeat(2)
			println(a[0])
			println(a[1])
			println(a[2])
			println(a[3])
		}
		{
			a := ['1', 'abc'].repeat(2)
			println(a[0])
			println(a[1])
			println(a[2])
			println(a[3])
		}
		{
			mut a := ['1', 'abc'].repeat(0)
			println(a.len)
			a << 'abc'
			println(a[0])
		}
	}
	{
		// todo(playX): deep repeat does not yet work.
		/*
		// test deep repeat
		mut a3 := [[[1, 1], [2, 2], [3, 3]], [[4, 4], [5, 5], [6, 6]]]
		r := a3.repeat(3)
		a3[1][1][0] = 17
		print(r)
		assert r == [
			[[1, 1], [2, 2], [3, 3]],
			[[4, 4], [5, 5], [6, 6]],
			[[1, 1], [2, 2], [3, 3]],
			[[4, 4], [5, 5], [6, 6]],
			[[1, 1], [2, 2], [3, 3]],
			[[4, 4], [5, 5], [6, 6]],
		]
		assert a3 == [[[1, 1], [2, 2], [3, 3]], [[4, 4], [17, 5],
			[6, 6],
		]]
		*/
	}
	{
		// test right
		a := [1, 2, 3, 4]
		c := a[1..a.len]
		d := a[1..]
		println(c[0])
		println(c[1])
		println(d[0])
		println(d[1])
	}
	{
		// test left
		a := [1, 2, 3]
		c := a[0..2]
		d := a[..2]
		println(c[0])
		println(c[1])
		println(d[0])
		println(d[1])
	}
	{
		// test slice
		a := [1, 2, 3, 4]
		b := a[2..4]
		println(b.len)
		println(a[1..2].len)
		println(a.len)
	}
	{
		// test push many
		mut a := [1, 2, 3]
		b := [4, 5, 6]
		a << b
		println(a.len)
		println(a[0])
		println(a[3])
		println(a[5])
	}
	{
		// test reverse
		a := [1, 2, 3, 4]
		b := ['test', 'array', 'reverse']
		c := a.reverse()
		println(c)
		d := b.reverse()
		for i, _ in c {
			println(c[i] == a[a.len - i - 1])
		}
		for i, _ in d {
			println(d[i] == b[b.len - i - 1])
		}
		e := []int{}
		f := e.reverse()
		println(f.len)
	}
	{
		// test fixed
		mut nums := [4]int{}
		// x := nums[1..3]
		// assert x.len == 2

		println(nums)
		nums[1] = 7
		println(nums)
		nums2 := [5]int{} // c_n
		println(nums2[c_n - 1])
	}
	{
		// test mut slice
		/*
		todo(playX): slices do not work yet. We have to implement custom wrapper for them.
		mut n := [1, 2, 3]
		// modify(mut n)
		modify(mut n[..2])
		assert n[0] == 777
		modify(mut n[2..])
		assert n[2] == 777
		println(n)
		*/
	}
	{
		// test mut arg
		mut arr := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
		double_up(mut arr)
		println(arr.str())
		arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
		double_up_v2(mut arr)
		println(arr.str())
	}
	{
		// test doubling
		mut nums := [1, 2, 3, 4, 5]
		for i in 0 .. nums.len {
			nums[i] *= 2
		}
		println(nums.str())
	}
	{
		// test single element

		mut a := [1]
		a << 2
		println(a.len)
		assert a[0] == 1
		assert a[1] == 2
		println(a)
	}
	{
		// test find index

		// string
		a := ['v', 'is', 'great']
		println(a.index('v'))
		println(a.index('is'))
		println(a.index('gre'))
		// int
		b := [1, 2, 3, 4]
		println(b.index(1))
		println(b.index(4))
		println(b.index(5))
		// byte
		c := [0x22, 0x33, 0x55]
		println(c.index(0x22))
		println(c.index(0x55))
		println(c.index(0x99))
		// char
		d := [`a`, `b`, `c`]
		println(d.index(`b`))
		println(d.index(`c`))
		println(d.index(`u`))
	}
	{
		// test multi

		a := [[1, 2, 3], [4, 5, 6]]
		println(a.len)
		println(a[0].len)
		println(a[0][0])
		println(a[0][2])
		println(a[1][2])
	}
	{
		// test in
		a := [1, 2, 3]
		println(1 in a)
		println(2 in a)
		println(3 in a)
		println(4 !in a)
		println(0 !in a)
		println(0 !in a)
		println(4 !in a)
		b := [1, 4, 0]
		c := [3, 6, 2, 0]
		println(0 in b)
		println(0 in c)
	}
	{
		// test reduce
		a := [1, 2, 3, 4, 5]
		b := a.reduce(sum, 0)
		c := a.reduce(sum, 5)
		d := a.reduce(sum, -1)
		println(b)
		println(c)
		println(d)
		e := [1, 2, 3]
		f := e.reduce(sub, 0)
		g := e.reduce(sub, -1)
		println(f)
		println(g)
	}
	{
		a := [1, 2, 3, 4, 5, 6]
		b := a.filter(it % 2 == 0)
		println(b)

		c := ['v', 'is', 'awesome']
		d := c.filter(it.len > 1)
		println(d)
		assert d[0] == 'is'
		assert d[1] == 'awesome'
		////////
		arr := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
		println(arr.filter(it % 2 == 0 || it % 3 == 0))

		mut mut_arr := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
		mut_arr = mut_arr.filter(it < 4)
		assert mut_arr.len == 3
		println(a.filter(filter_test_helper_1))
		println([1, 5, 10].filter(filter_test_helper_1))
	}
	{
		// test anon fn filter
		filter_num := fn (i int) bool {
			return i % 2 == 0
		}
		println([1, 2, 3, 4, 5].filter(filter_num))
	}
	{
		a := [1, 2, 3, 4].filter(fn (i int) bool {
			return i % 2 == 0
		})
		println(a)
	}
	{
		// test map
		nums := [1, 2, 3, 4, 5, 6]
		strs := ['v', 'is', 'awesome']
		// assert nums.map() == <error>
		// assert nums.map(it, 'excessive') == <error>
		// identity
		println(nums.map(it))
		println(strs.map(it))
		println(nums.map(it - it))
		println(nums.map(it - it)[0])
		// type switch
		println(nums.map(it * 10))
		println(nums.map(it * it))
		println(nums.map('$it'))
		println(nums.map(it % 2 == 0))
		println(strs.map(it.to_upper()))
		println(strs.map(it == 'awesome'))
		println(strs.map(it.len in nums))
		println(strs.map(int(7)))
		// external func
		println(nums.map(map_test_helper_1(it)))
		println(nums.map(map_test_helper_2(it, 'bb')))
		println(nums.map(map_test_helper_3(it, strs)))
		// empty array as input
		println([]int{len: 0}.map(it * 2))
		// nested maps (where it is of same type)
		println(nums.map(strs.map(int(7)) == [7, 7, 7]))
		println(nums.map('$it' + strs.map('a')[0]))
		// assert nums.map(it + strs.map(int(7))[0]) == [8, 9, 10, 11, 12, 13]
		println(nums.map(it + strs.map(it.len)[0]))
		println(strs.map(it.len + strs.map(it.len)[0]))
		// nested (different it types)
		// todo(playX): this one produces invalid JS code.
		// assert strs.map(it[nums.map(it - it)[0]]) == [byte(`v`), `i`, `a`]
		println(nums[0..3].map('$it' + strs.map(it)[it - 1]))
		println(nums.map(map_test_helper_1))
		println([1, 5, 10].map(map_test_helper_1))
		println(nums)
		println(strs)
	}
	{
		// test anon fn map
		add_num := fn (i int) int {
			return i + 1
		}
		println([1, 2, 3].map(add_num))
	}
	{
		// test multi anon fn map
		a := [1, 2, 3].map(fn (i int) int {
			return i + 1
		})
		b := [1, 2, 3].map(fn (i int) int {
			return i + 2
		})
		println(a)
		println(b)
	}
	{
		// test anon fn arg map
		a := [1, 2, 3].map(fn (i int) int {
			return i + 1
		})
		println(a)
	}
	{
		// test anon fn arg different type map
		i_to_str := fn (i int) string {
			return i.str()
		}
		a := [1, 2, 3].map(i_to_str)
		println(a)
	}
	{
		// test anon fn inline different type map
		a := [1, 2, 3].map(fn (i int) string {
			return i.str()
		})
		println(a)
	}
	{
		// test array str

		numbers := [1, 2, 3]
		assert numbers == [1, 2, 3]
		numbers2 := [numbers, [4, 5, 6]] // dup str() bug
		println(numbers2)
		assert true
		assert numbers.str() == '[1, 2, 3]'
	}
	{
		// test eq
		println([5, 6, 7] != [6, 7])
		println([`a`, `b`] == [`a`, `b`])
		println([User{
			age: 22
			name: 'bob'
		}] == [User{
			age: 22
			name: 'bob'
		}])
		// todo(playX): map cmp does not work yet
		/*
		assert [map{
			'bob': 22
		}, map{
			'tom': 33
		}] == [map{
			'bob': 22
		}, map{
			'tom': 33
		}]*/
		println([[1, 2, 3], [4]] == [[1, 2, 3], [4]])
	}
	{
		// test fixed array eq
		a1 := [1, 2, 3]!
		println(a1 == [1, 2, 3]!)
		println(a1 != [2, 3, 4]!)

		a2 := [[1, 2]!, [3, 4]!]!
		println(a2 == [[1, 2]!, [3, 4]!]!)
		println(a2 != [[3, 4]!, [1, 2]!]!)

		a3 := [[1, 2], [3, 4]]!
		println(a3 == [[1, 2], [3, 4]]!)
		println(a3 != [[1, 1], [2, 2]]!)

		a4 := [[`a`, `b`], [`c`, `d`]]!
		println(a4 == [[`a`, `b`], [`c`, `d`]]!)
		println(a4 != [[`c`, `a`], [`a`, `b`]]!)

		a5 := [['aaa', 'bbb'], ['ccc', 'ddd']]!
		println(a5 == [['aaa', 'bbb'], ['ccc', 'ddd']]!)
		println(a5 != [['abc', 'def'], ['ccc', 'ddd']]!)

		a6 := [['aaa', 'bbb']!, ['ccc', 'ddd']!]!
		println(a6 == [['aaa', 'bbb']!, ['ccc', 'ddd']!]!)
		println(a6 != [['aaa', 'bbb']!, ['aaa', 'ddd']!]!)

		a7 := [[1, 2]!, [3, 4]!]
		println(a7 == [[1, 2]!, [3, 4]!])
		println(a7 != [[2, 3]!, [1, 2]!])

		a8 := [['aaa', 'bbb']!, ['ccc', 'ddd']!]
		println(a8 == [['aaa', 'bbb']!, ['ccc', 'ddd']!])
		println(a8 != [['bbb', 'aaa']!, ['cccc', 'dddd']!])
	}
	{
		// test fixed array literal eq
		println([1, 2, 3]! == [1, 2, 3]!)
		println([1, 1, 1]! != [1, 2, 3]!)

		println([[1, 2], [3, 4]]! == [[1, 2], [3, 4]]!)
		println([[1, 1], [2, 2]]! != [[1, 2], [3, 4]]!)

		println([[1, 1]!, [2, 2]!]! == [[1, 1]!, [2, 2]!]!)
		println([[1, 1]!, [2, 2]!]! != [[1, 2]!, [2, 3]!]!)

		println([[1, 1]!, [2, 2]!] == [[1, 1]!, [2, 2]!])
		println([[1, 1]!, [2, 2]!] != [[1, 2]!, [2, 3]!])
	}
	{
		// test sort
		mut a := ['hi', '1', '5', '3']
		a.sort()
		println(a)

		mut nums := [67, -3, 108, 42, 7]
		nums.sort()
		println(nums)
		assert nums[0] == -3
		assert nums[1] == 7
		assert nums[2] == 42
		assert nums[3] == 67
		assert nums[4] == 108
		// todo(playX): add codegen for comparator fn passed
		/*
		nums.sort(a < b)
		assert nums[0] == -3
		assert nums[1] == 7
		assert nums[2] == 42
		assert nums[3] == 67
		assert nums[4] == 108

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

		users.sort(a.name < b.name) // Test sorting by string fields
		*/
	}
	{
		/*
		// test rune sort
		mut bs := [`f`, `e`, `d`, `b`, `c`, `a`]
		bs.sort()
		println(bs)*/

		/*
		bs.sort(a > b)
		println(bs)
		assert '$bs' == '[`f`, `e`, `d`, `c`, `b`, `a`]'

		bs.sort(a < b)
		println(bs)
		assert '$bs' == '[`a`, `b`, `c`, `d`, `e`, `f`]'
		*/
	}
	{
		// test f32 sort
		mut f := [f32(50.0), 15, 1, 79, 38, 0, 27]
		f.sort()
		println(f[0])
		println(f[1])
		println(f[6])
	}
	{
		// test f64 sort
		mut f := [50.0, 15, 1, 79, 38, 0, 27]
		f.sort()
		println(f)
		assert f[0] == 0.0
		assert f[1] == 1.0
		assert f[6] == 79.0
	}
	{
		// test i64 sort
		mut f := [i64(50), 15, 1, 79, 38, 0, 27]
		f.sort()
		println(f)
		assert f[0] == 0
		assert f[1] == 1
		assert f[6] == 79
	}
	{
		// test in struct

		mut baz := Foo{
			bar: [0, 0, 0]
		}
		baz.bar[0] += 2
		baz.bar[0]++
		println(baz.bar[0])
	}
	{
		// test direct modification
		mut foo := [2, 0, 5]
		foo[1] = 3
		foo[0] *= 7
		foo[1]--
		foo[2] -= 2
		println(foo)
	}
	{
		// test bools
		println('test b')
		mut a := [true, false]
		a << true
		println(a)
	}
	{
		// test push many self
		mut actual_arr := [1, 2, 3, 4]
		actual_arr << actual_arr
		expected_arr := [1, 2, 3, 4, 1, 2, 3, 4]
		assert actual_arr.len == expected_arr.len
		for i in 0 .. actual_arr.len {
			print(actual_arr[i])
			print(',')
			println(expected_arr[i])
		}
	}
	{
		// test for
		nums := [1, 2, 3]
		mut sum := 0
		for num in nums {
			sum += num
		}
		println(sum)
	}
	{
		// test left shift precedence

		mut arr := []int{}
		arr << 1 + 1
		arr << 1 - 1
		arr << 2 / 1
		arr << 2 * 1
		println(arr)
	}
	{
		// todo(playX): what we should do with cap on the JS backend?
		/*
		// test array with cap
		a4 := []int{len: 1, cap: 10}
		println(a4.cap)
		assert a4.len == 1
		assert a4.cap == 10
		a5 := []int{len: 1, cap: 10}
		assert a5.len == 1
		assert a5.cap == 10
		*/
	}
	{
		// test multi array index
		mut a := [][]int{len: 2, init: []int{len: 3, init: 0}}
		a[0][0] = 1
		println('$a')
		mut b := [[0].repeat(3)].repeat(2)
		b[0][0] = 1
		println('$b')
	}
	{
		// test plus assign string
		mut a := ['']
		a[0] += 'abc'

		println(a)
	}
	{
		// test mut arr with eq in fn
		mut a := [1, 2, 3, 4]
		mut_arr_with_eq_in_fn(mut a)
		println(a)
	}
	{
		// test array in mut
		mut a := [1, 2]
		array_in_mut(mut a)
		println(a)
	}
	{
		mut nums := [1, 2, 3]
		add_nums(mut nums)
		println(nums)
	}
	{
		// test reverse in place
		mut a := [1, 2, 3, 4]
		a.reverse_in_place()
		println(a)
		mut b := ['a', 'b', 'c']
		b.reverse_in_place()
		println(b)
		mut c := [[1, 2], [3, 4], [5, 6]]
		c.reverse_in_place()
		println(c)
	}
	{
		// test array int pop
		mut a := [1, 2, 3, 4, 5]
		assert a.len == 5
		x := a.last()
		y := a.pop()
		println('$x,$y')
		assert a.len == 4
		z := a.pop()
		assert a.len == 3
		println(z)
		a.pop()
		a.pop()
		final := a.pop()
		println(final)
	}
	{
		// test array string pop
		mut a := ['abc', 'def', 'xyz']
		assert a.len == 3
		println(a.pop())
		println(a.pop())
		println(a.pop())
		assert a.len == 0
	}
	{
		// test array first
		a := [3]
		println(a.first())
		b := [1, 2, 3, 4]
		println(b.first())
		c := ['abc', 'def']
		println(c.first())
		// todo(playX): we should implement byte str cmp

		s := [Chunk{'a'}]
		println(s.first().val)
	}
	{
		// test array last
		a := [3]
		println(a.last())
		b := [1, 2, 3, 4]
		println(b.last())
		c := ['abc', 'def']
		println(c.last())
		s := [Chunk{'a'}]
		println(s.last().val)
	}
	{
		// test direct array access
		mut a := [11, 22, 33, 44]
		println(a[0])
		println(a[2])
		x := a[0]
		a[0] = 21
		a[1] += 2
		a[2] = x + 3
		a[3] -= a[1]
		println(a)
	}
	{
		// test multidimensional array initialization with consts
		mut data := [][][]int{len: grid_size_1, init: [][]int{len: grid_size_2, init: []int{len: grid_size_3, init: cell_value}}}
		println(data.len)
		println(data[0].len)
		println(data[0][0].len)
		println(data[0][0][0])
		println(data[1][1][1])
	}
	{
		// test multi array prepend
		mut a := [][]int{}
		a.prepend([1, 2, 3])
		println(a)
		mut b := [][]int{}
		b.prepend([[1, 2, 3]])
		println(b)
	}
	{
		// test multi array insert
		mut a := [][]int{}
		a.insert(0, [1, 2, 3])
		println(a)
		mut b := [][]int{}
		b.insert(0, [[1, 2, 3]])
		println(b)
	}
	{
		// test multi array in
		a := [[1]]
		println([1] in a)
	}
	{
		// test any type array contains
		a := [true, false]
		println(a.contains(true))
		println(true in a)
		println(a.contains(false))
		println(false in a)
		b := [i64(2), 3, 4]
		println(b.contains(i64(3)))
		println(5 !in b)
		c := [[1], [2]]
		println(c.contains([1]))
		println([2] in c)
		println([3] !in c)
	}
	{
		// test struct array of multi type in
		ivan := Person{
			name: 'ivan'
			nums: [1, 2, 3]
			kv: {
				'aaa': '111'
			}
		}
		people := [Person{
			name: 'ivan'
			nums: [1, 2, 3]
			kv: {
				'aaa': '111'
			}
		}, Person{
			name: 'bob'
			nums: [2]
			kv: {
				'bbb': '222'
			}
		}]
		println(ivan in people)
	}
	{
		// test struct array of multi type index
		ivan := Person{
			name: 'ivan'
			nums: [1, 2, 3]
			kv: {
				'aaa': '111'
			}
		}
		people := [Person{
			name: 'ivan'
			nums: [1, 2, 3]
			kv: {
				'aaa': '111'
			}
		}, Person{
			name: 'bob'
			nums: [2]
			kv: {
				'bbb': '222'
			}
		}]
		println(people.index(ivan))
		assert people.index(ivan) == 0
	}
	{
		// test array struct contains

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
	{
		// test array of array append
		mut x := [][]int{len: 4}
		println(x) // OK
		x[2] << 123 // RTE
		println(x)
	}
	{
		// test array of map insert
		mut x := []map[string]int{len: 4}
		println(x) // OK
		x[2]['123'] = 123 // RTE
		println(x)
	}
	{
		// todo(playX): does not work
		/*
		// test multi fixed array init
		a := [3][3]int{}
		println(a)
		*/
	}
	{
		// test array of multi filter
		arr := [1, 2, 3, 4, 5]
		nums := Numbers{
			odds: arr.filter(it % 2 == 1)
			evens: arr.filter(it % 2 == 0)
		}
		println(nums)
		assert nums.odds == [1, 3, 5]
		assert nums.evens == [2, 4]
	}
	{
		// test array of multi map
		arr := [1, 3, 5]
		nums := Numbers{
			odds: arr.map(it + 2)
			evens: arr.map(it * 2)
		}
		println(nums)
		assert nums.odds == [3, 5, 7]
		assert nums.evens == [2, 6, 10]
	}
	{
		// test multi fixed array with default init
		a := [3][3]int{init: [3]int{init: 10}}
		println(a)
		assert a == [[10, 10, 10]!, [10, 10, 10]!, [10, 10, 10]!]!
	}
}
