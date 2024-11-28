struct Foo1 {
	arr1 [5]?int
	arr2 [5]int
}

fn get_has_option_fixed() ?int {
	foo := Foo1{}
	return foo.arr1[0]
}

fn get_no_option_fixed() int {
	foo := Foo1{}
	return foo.arr2[0]
}

fn test_option_fixed() {
	x := get_has_option_fixed() or { 0 }
	assert x == 0
	assert get_no_option_fixed() == 0
}

struct Foo2 {
mut:
	arr1 []?int
	arr2 []int
}

fn get_has_option() ?int {
	mut foo := Foo2{}
	foo.arr1 << 0
	return foo.arr1[0]
}

fn get_no_option() int {
	mut foo := Foo2{}
	foo.arr2 << 0
	return foo.arr2[0]
}

fn test_option_non_fixed() {
	x := get_has_option()?
	assert x == 0
	assert get_no_option() == 0
}
