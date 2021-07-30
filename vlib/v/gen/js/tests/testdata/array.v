struct Chunk {
	val string
}

struct Kkk {
	q []Chunk
}

const (
	c_n = 5
)

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
		println(!(4 in a))
		println(!(0 in a))
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
		// todo(playX): RUNTIME ERROR: Invalid memory access
		/*
		// test anon fn filter
		filter_num := fn (i int) bool {
			return i % 2 == 0
		}
		assert [1, 2, 3, 4, 5].filter(filter_num) == [2, 4]
		*/
	}
	{
		/*
		a := [1, 2, 3, 4].filter(fn (i int) bool {
			return i % 2 == 0
		})
		assert a == [2, 4]
		*/
	}
}
