fn foo1(mut arr []int) {
	arr[0], arr[1] = arr[1], arr[0]
}

fn test_array_cross_assign_in_fn() {
	mut arr := [1,2]
	foo1(mut arr)
	assert arr[0] == 2
	assert arr[1] == 1
}

fn foo2(mut a map[string]int) {
	a['one'], a['two'] = a['two'], a['one']
}

fn test_map_cross_assign_in_fn() {
	mut a := {'one':1, 'two':2}
	foo2(mut a)
	assert a['one'] == 2
	assert a['two'] == 1
}

struct Foo {
mut:
	a int
	b int
}

fn foo3(mut f &Foo) {
	f.a, f.b = f.b, f.a
}

fn main() {
	mut a := Foo{a:1, b:2}
	foo3(mut a)
	println(a)
	assert a.a == 2
	assert a.b == 1
}