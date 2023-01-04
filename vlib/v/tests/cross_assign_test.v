import math.big

// Test cross assign of array elements
fn test_cross_assign_of_array() {
	mut a := [0, 1]
	a[0], a[1] = a[1], a[0]
	assert a[0] == 1
	assert a[1] == 0

	mut b1 := [1, 2, 3]
	mut b2 := 4
	b1[2], b2, b1[0] = b1[0], b1[2], 5
	assert b1 == [5, 2, 1]
	assert b2 == 3
}

// Test cross assign of array elements in function
fn foo1(mut arr []int) {
	arr[0], arr[1] = arr[1], arr[0]
}

fn test_cross_assign_of_array_in_fn() {
	mut arr := [1, 2]
	foo1(mut arr)
	assert arr[0] == 2
	assert arr[1] == 1
}

// Test cross assign of map values
fn test_cross_assign_of_map() {
	mut a := {
		'one': 1
		'two': 2
	}
	a['one'], a['two'] = a['two'], a['one']
	println(a)
	assert a['one'] == 2
	assert a['two'] == 1
}

// Test cross assign of map values in function
fn foo2(mut a map[string]int) {
	a['one'], a['two'] = a['two'], a['one']
}

fn test_cross_assign_of_map_in_fn() {
	mut a := {
		'one': 1
		'two': 2
	}
	foo2(mut a)
	assert a['one'] == 2
	assert a['two'] == 1
}

// Test cross assign of struct fields
struct Zoo {
mut:
	a int
	b int
}

fn test_cross_assign_of_struct() {
	mut x := Zoo{
		a: 1
		b: 2
	}
	x.a, x.b = x.b, x.a
	// println(x)
	assert x.a == 2
	assert x.b == 1
}

// Test cross assign of struct fields in function
struct Foo {
mut:
	a int
	b int
}

fn foo3(mut f Foo) {
	f.a, f.b = f.b, f.a
}

fn test_cross_assign_of_struct_in_fn() {
	mut a := Foo{
		a: 1
		b: 2
	}
	foo3(mut a)
	println(a)
	assert a.a == 2
	assert a.b == 1
}

// Test cross assign of mixed types
fn test_cross_assign_of_mixed_types() {
	mut a := [0, 1]
	mut m := {
		'one': 1
		'two': 2
	}
	mut x := Zoo{
		a: 1
		b: 2
	}

	a[0], m['one'], x.a, a[1], m['two'], x.b = a[1], m['two'], x.b, a[0], m['one'], x.a

	assert a == [1, 0]
	assert m['one'] == 2
	assert m['two'] == 1
	assert x.a == 2
	assert x.b == 1
}

// Test cross assign of mixed types in function
fn foo(mut a []int, mut m map[string]int, mut x Zoo) {
	a[0], m['one'], x.a, a[1], m['two'], x.b = a[1], m['two'], x.b, a[0], m['one'], x.a
}

fn test_cross_assign_of_mixed_types_in_fn() {
	mut a := [0, 1]
	mut m := {
		'one': 1
		'two': 2
	}
	mut x := Zoo{
		a: 1
		b: 2
	}

	foo(mut a, mut m, mut x)

	assert a == [1, 0]
	assert m['one'] == 2
	assert m['two'] == 1
	assert x.a == 2
	assert x.b == 1
}

// Test cross assign of complex types
fn test_cross_assign_of_complex_types() {
	mut a := [0, 1]
	mut m := {
		'one': 1
		'two': 2
	}
	mut x := Zoo{
		a: 1
		b: 2
	}

	a[0], m['one'], x.a, a[1], m['two'], x.b = a[1] + 1, -m['two'], x.b, a[0] * 2, m['one'] * 3, x.a - x.b

	assert a == [2, 0]
	assert m['one'] == -2
	assert m['two'] == 3
	assert x.a == 2
	assert x.b == -1
}

fn test_cross_assign_of_big_int() {
	mut a := big.zero_int
	mut b := big.one_int

	a, b = a + b, a
	println(a)
	assert a == big.one_int
}

fn test_cross_assign_of_reserved_name_variable() {
	mut small := 1
	mut big := 2
	mut sum := 2

	for big < 4_000_000 {
		small, big = big, small + big
		if big % 2 == 0 {
			sum += big
		}
	}
	println(small)
	assert small == 3524578

	println(big)
	assert big == 5702887

	println(sum)
	assert sum == 4613732
}
