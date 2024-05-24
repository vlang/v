const size = 5
const u64_size = u64(5)
const int_size = int(1)
const infix_cast_size = int(100 / 50)
const int_sizeof_cast_size = ((1600 / 8) / int(sizeof(u64)))

struct Foo {
	bar [size]u8
}

fn test_fixed_array_const_size() {
	a := Foo{}
	println(a)
	assert a == Foo{
		bar: [u8(0), 0, 0, 0, 0]!
	}

	b := [int_size]int{}
	assert b.len == 1
	assert b == [0]!

	c := [infix_cast_size]int{}
	assert c.len == 2
	assert c == [0, 0]!
}

fn test_fixed_array_const_u64_size() {
	a := [2 * u64_size]f64{}
	println(a)
	assert '${a}' == '[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]'
}

const n = u64(5_000)

const nn = 5_000

fn test_u64_const_used_as_fixed_array_size() {
	mut a := [2 * n]f64{}
	dump(a.len)
	assert a.len == 10000

	mut b := [n * 2]f64{}
	dump(b.len)
	assert b.len == 10000
}

fn test_int_const_used_as_fixed_array_size() {
	mut aa := [2 * nn]f64{}
	dump(aa.len)
	assert aa.len == 10_000

	mut bb := [nn * 2]f64{}
	dump(bb.len)
	assert aa.len == 10_000
}

const rows = 2
const cols = 3

struct Matrix1 {
	data [rows * cols]int
}

struct Matrix2 {
	data [rows * cols + 4]int
}

struct Matrix3 {
	data [rows * 4 - 2 * cols]int
}

struct Matrix4 {
	data [5 * rows - cols * 2]int
}

struct Matrix5 {
	data [5 * rows / 2]int
}

struct Matrix6 {
	data [5 * rows / 2 + 1]int
}

fn test_infix_const_expr_used_as_fixed_array_size() {
	mat1 := Matrix1{}
	println(mat1)
	assert mat1.data.len == 6

	mat2 := Matrix2{}
	println(mat2)
	assert mat2.data.len == 10

	mat3 := Matrix3{}
	println(mat3)
	assert mat3.data.len == 2

	mat4 := Matrix4{}
	println(mat4)
	assert mat4.data.len == 4

	mat5 := Matrix5{}
	println(mat5)
	assert mat5.data.len == 5

	mat6 := Matrix6{}
	println(mat6)
	assert mat6.data.len == 6
}

fn test_int_sizeof_size() {
	arr_fixed := [int_sizeof_cast_size]int{}
	assert arr_fixed.len == 25
}
