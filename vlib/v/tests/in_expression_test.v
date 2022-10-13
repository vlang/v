enum Colors {
	red
	green
	blue
	yellow
}

fn test_in_expression() {
	mut a := false
	arr1 := [1, 2]
	arr2 := [0, 2]
	arr3 := [1, 0]

	a = true && 2 in arr1
	assert a == true
	a = false && 2 in arr1
	assert a == false

	a = true && 0 in arr2
	assert a == true
	a = false && 0 in arr3
	assert a == false
	a = true && 0 in arr1
	assert a == false
	a = true && 3 in arr1
	assert a == false
	a = true && 2 !in arr2
	assert a == false
	a = true && 3 !in arr2
	assert a == true

	a = true && 2 !in arr2
	assert a == false
	a = true && 3 !in arr2
	assert a == true

	a = 1 in arr1 && true
	assert a == true
	a = 1 in arr1 && false
	assert a == false
}

/*
not implemented
fn test_in_expression_with_enum() {
	mut a := false
	arr1 := [Colors.green, .blue]
	arr2 := [Colors.red, .blue]
	arr3 := [Colors.green, .red]
	a = true && Colors.blue in arr1
	assert a == true
	a = false && Colors.blue in arr1
	assert a == false
	a = true && Colors.red in arr2
	assert a == true
	a = false && Colors.red in arr3
	assert a == false

	a = true && Colors.red in arr1
	assert a == false
	a = true && Colors.yellow in arr1
	assert a == false

	a = true && !(Colors.blue in arr2)
	assert a == false
	a = true && !(Colors.yellow in arr2)
	assert a == true

	a = Colors.green in arr1 && true
	assert a == true
	a = Colors.green in arr1 && false
	assert a == false
}
*/
fn test_in_expression_with_string() {
	mut a := false
	arr1 := ['ab', 'bc']
	arr2 := ['', 'bc']
	arr3 := ['ab', '']

	a = true && 'bc' in arr1
	assert a == true
	a = false && 'bc' in arr1
	assert a == false

	a = true && '' in arr2
	assert a == true
	a = false && '' in arr3
	assert a == false
	a = true && '' in arr1
	assert a == false
	a = true && 'abc' in arr1
	assert a == false
	a = true && 'bc' !in arr2
	assert a == false
	a = true && 'abc' !in arr2
	assert a == true

	a = true && 'bc' !in arr2
	assert a == false
	a = true && 'abc' !in arr2
	assert a == true

	a = 'ab' in arr1 && true
	assert a == true
	a = 'ab' in arr1 && false
	assert a == false
}

type MapAlias = map[string]int
type ArrayAlias = []int

fn test_in_expression_in_alias() {
	arr := ArrayAlias([0, 1])
	assert 0 in arr
	assert 100 !in arr

	m := MapAlias({
		'one':   1
		'two':   2
		'three': 3
	})
	assert 'one' in m
	assert 'four' !in m
}

fn test_in_expression_in_map() {
	m := {
		'one':   1
		'two':   2
		'three': 3
	}
	assert 'one' in m
	assert 'four' !in m
}

fn test_optimized_in_expression() {
	mut a := false
	a = true && 2 in [1, 2]
	assert a == true
	a = false && 2 in [1, 2]
	assert a == false

	a = true && 0 in [0, 2]
	assert a == true
	a = false && 0 in [1, 0]
	assert a == false
	a = true && 0 in [1, 2]
	assert a == false
	a = true && 3 in [1, 2]
	assert a == false
	a = true && 2 !in [0, 2]
	assert a == false
	a = true && 3 !in [0, 2]
	assert a == true

	a = true && 2 !in [0, 2]
	assert a == false
	a = true && 3 !in [0, 2]
	assert a == true

	a = 1 in [1, 2] && true
	assert a == true
	a = 1 in [1, 2] && false
	assert a == false
}

fn test_optimized_in_expression_with_enum() {
	mut a := false
	a = true && Colors.blue in [.green, .blue]
	assert a == true
	a = false && Colors.blue in [.green, .blue]
	assert a == false

	a = true && Colors.red in [.red, .blue]
	assert a == true
	a = false && Colors.red in [.green, .red]
	assert a == false
	a = true && Colors.red in [.green, .blue]
	assert a == false
	a = true && Colors.yellow in [.green, .blue]
	assert a == false
	a = true && Colors.blue !in [.red, .blue]
	assert a == false
	a = true && Colors.yellow !in [.red, .blue]
	assert a == true

	a = true && Colors.blue !in [.red, .blue]
	assert a == false
	a = true && Colors.yellow !in [.red, .blue]
	assert a == true

	a = Colors.green in [.green, .blue] && true
	assert a == true
	a = Colors.green in [.green, .blue] && false
	assert a == false
}

fn test_optimized_in_expression_with_string() {
	mut a := false
	a = true && 'bc' in ['ab', 'bc']
	assert a == true
	a = false && 'bc' in ['ab', 'bc']
	assert a == false

	a = true && '' in ['', 'bc']
	assert a == true
	a = false && '' in ['ab', '']
	assert a == false
	a = true && '' in ['ab', 'bc']
	assert a == false
	a = true && 'abc' in ['ab', 'bc']
	assert a == false
	a = true && 'bc' !in ['', 'bc']
	assert a == false
	a = true && 'abc' !in ['', 'bc']
	assert a == true

	a = true && 'bc' !in ['', 'bc']
	assert a == false
	a = true && 'abc' !in ['', 'bc']
	assert a == true

	a = 'ab' in ['ab', 'bc'] && true
	assert a == true
	a = 'ab' in ['ab', 'bc'] && false
	assert a == false
}

fn test_in_array_init() {
	assert 1 !in []int{}
	assert [1] in [[1], [2]]
}

fn test_in_expression_numeric() {
	b := [u8(2), 4, 7]
	b2 := [i8(3), -4, 9]
	s := [u16(6), 1, 0]
	s2 := [i16(34), -17, 45]
	i := [5, 7, 9]
	i2 := [u32(65), 12, 9]
	l := [u64(54), 23, 1]
	l2 := [i64(-45), 8, 2]
	f := [f32(12.5), 0, -17.25]
	f2 := [1.0625, 3, 17.125]
	assert u8(4) in b
	assert 3 !in b
	assert -4 in b2
	assert i8(5) !in b2
	assert 1 in s
	assert u16(3) !in s
	assert 45 in s2
	assert i16(0) !in s2
	assert 7 in i
	assert 8 !in i
	assert 12 in i2
	assert u32(13) !in i2
	assert u64(1) in l
	assert 2 !in l
	assert -45 in l2
	assert i64(-17) !in l2
	assert -17.25 in f
	assert f32(1) !in f
	assert 1.0625 in f2
	assert 3.5 !in f2
}

struct Foo1 {}

struct Foo2 {}

struct Foo3 {}

type Foo = Foo1 | Foo2 | Foo3

fn test_in_sumtype_array() {
	foo := Foo(Foo3{})

	if foo in [Foo1, Foo3] {
		println(foo)
		assert true
	}

	// without sumtype cast
	mut foos := []Foo{}
	foos << Foo1{}
	assert Foo1{} in foos
	assert Foo2{} !in foos
}

fn test_in_struct_array() {
	assert Foo1{} == Foo1{}
}

fn fn1() {}

fn fn2() {}

fn fn3() {}

fn test_in_func_array() {
	assert fn1 in [fn1, fn2, fn3]
}

type Str = string
type Struct = Foo1

fn test_in_alias_array() {
	assert Str('') in [Str(''), Str('a')]
	assert Struct{} == Struct{}
}

type TokenValue = rune | u64

fn test_in_array_literal_of_sumtype() {
	val1 := TokenValue(`+`)
	assert val1 in [TokenValue(`+`), TokenValue(`-`)]

	val2 := `+`
	assert val2 in [TokenValue(`+`), TokenValue(`-`)]
}

type StringOrNumber = int | string

fn test_in_array_of_sumtype() {
	arr := [StringOrNumber(1), 2, 'test']
	println(1 in arr)
	assert 1 in arr
}
