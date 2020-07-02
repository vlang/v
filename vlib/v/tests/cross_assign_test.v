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
	mut arr := [1,2]
	foo1(mut arr)
	assert arr[0] == 2
	assert arr[1] == 1
}

// Test cross assign of map values
fn test_cross_assign_of_map() {
	mut a := {'one':1, 'two':2}
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
	mut a := {'one':1, 'two':2}
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
	mut x := Zoo{a:1, b:2}
	x.a, x.b = x.b, x.a
	//println(x)
	assert x.a == 2
	assert x.b == 1
}

// Test cross assign of struct fields in function
struct Foo {
mut:
	a int
	b int
}

fn foo3(mut f &Foo) {
	f.a, f.b = f.b, f.a
}

fn test_cross_assign_of_struct_in_fn() {
	mut a := Foo{a:1, b:2}
	foo3(mut a)
	println(a)
	assert a.a == 2
	assert a.b == 1
}
