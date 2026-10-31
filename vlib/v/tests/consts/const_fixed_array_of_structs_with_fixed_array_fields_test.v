// Regression test for https://github.com/vlang/v/issues/28900 .
// The `!` of `[...]!` fixed array literals used as struct field values
// must not leak into the C initializer of a const fixed array of structs.
@[has_globals]
module main

struct Foo {
	a [4]u8
	b [4]u8
}

struct Grid {
	cells [2][3]int
	id    int
}

struct Inner {
	a [2]u8
	n int
}

struct Outer {
	items [2]Inner
	tag   int
}

struct Named {
	names [2]string
	inner Foo
}

fn runtime_n() int {
	return 5
}

const foos = [
	Foo{
		a: [u8(0), 0, 0, 0]!
		b: [u8(255), 255, 255, 255]!
	},
	Foo{
		a: [u8(1), 2, 3, 4]!
		b: [u8(5), 6, 7, 8]!
	},
]!

const single_foo = Foo{
	a: [u8(9), 8, 7, 6]!
	b: [u8(5), 4, 3, 2]!
}

const dynamic_foos = [
	Foo{
		a: [u8(10), 20, 30, 40]!
		b: [u8(50), 60, 70, 80]!
	},
]

const grids = [
	Grid{
		cells: [[1, 2, 3]!, [4, 5, 6]!]!
		id:    7
	},
	Grid{
		id: 8
	},
]!

const base = u8(10)
const fixed_b = [u8(7), 6, 5, 4]!

const foos_from_consts = [
	Foo{
		a: [base, base + 1, base * 2, base - 1]!
		b: fixed_b
	},
]!

const outers = [
	Outer{
		items: [Inner{
			a: [u8(11), 12]!
			n: 13
		}, Inner{
			a: [u8(14), 15]!
			n: 16
		}]!
		tag:   1
	},
]!

// Elements that need runtime initialization must not break the static ones.
const mixed = [
	Inner{
		a: [u8(1), 2]!
		n: runtime_n()
	},
	Inner{
		a: [u8(3), 4]!
		n: 6
	},
]!

const named = [
	Named{
		names: ['a', 'bc']!
		inner: Foo{
			a: [u8(31), 32, 33, 34]!
			b: [u8(35), 36, 37, 38]!
		}
	},
]!

__global g_foos = [
	Foo{
		a: [u8(21), 22, 23, 24]!
		b: [u8(25), 26, 27, 28]!
	},
]!

fn test_const_fixed_array_of_structs_with_fixed_array_fields() {
	assert foos.len == 2
	assert foos[0].a == [u8(0), 0, 0, 0]!
	assert foos[0].b == [u8(255), 255, 255, 255]!
	assert foos[1].a == [u8(1), 2, 3, 4]!
	assert foos[1].b == [u8(5), 6, 7, 8]!
}

fn test_const_struct_with_fixed_array_fields() {
	assert single_foo.a == [u8(9), 8, 7, 6]!
	assert single_foo.b == [u8(5), 4, 3, 2]!
}

fn test_const_dynamic_array_of_structs_with_fixed_array_fields() {
	assert dynamic_foos.len == 1
	assert dynamic_foos[0].a == [u8(10), 20, 30, 40]!
	assert dynamic_foos[0].b == [u8(50), 60, 70, 80]!
}

fn test_const_fixed_array_of_structs_with_nested_fixed_array_fields() {
	assert grids[0].cells == [[1, 2, 3]!, [4, 5, 6]!]!
	assert grids[0].id == 7
	assert grids[1].cells == [[0, 0, 0]!, [0, 0, 0]!]!
	assert grids[1].id == 8
}

fn test_const_fixed_array_of_structs_with_const_expr_fields() {
	assert foos_from_consts[0].a == [u8(10), 11, 20, 9]!
	assert foos_from_consts[0].b == [u8(7), 6, 5, 4]!
}

fn test_const_fixed_array_of_structs_with_fixed_arrays_of_structs() {
	item0 := outers[0].items[0]
	item1 := outers[0].items[1]
	assert item0.a == [u8(11), 12]!
	assert item0.n == 13
	assert item1.a == [u8(14), 15]!
	assert item1.n == 16
	assert outers[0].tag == 1
}

fn test_const_fixed_array_of_structs_with_runtime_and_static_elements() {
	assert mixed[0].a == [u8(1), 2]!
	assert mixed[0].n == 5
	assert mixed[1].a == [u8(3), 4]!
	assert mixed[1].n == 6
}

fn test_const_fixed_array_of_structs_with_string_and_nested_struct_fields() {
	assert named[0].names == ['a', 'bc']!
	assert named[0].inner.a == [u8(31), 32, 33, 34]!
	assert named[0].inner.b == [u8(35), 36, 37, 38]!
}

fn test_global_fixed_array_of_structs_with_fixed_array_fields() {
	assert g_foos[0].a == [u8(21), 22, 23, 24]!
	assert g_foos[0].b == [u8(25), 26, 27, 28]!
}
