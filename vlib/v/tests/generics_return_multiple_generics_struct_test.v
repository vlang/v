struct Foo[A, B] {
mut:
	a A
	b B
}

fn new_foo[A, B](a A, b B) Foo[A, B] {
	return Foo[A, B]{
		a: a
		b: b
	}
}

fn get_a[A, B](opt Foo[A, B]) A {
	return opt.a
}

fn get_b[A, B](opt Foo[A, B]) B {
	return opt.b
}

fn set[A, B](mut opt Foo[A, B], a A, b B) {
	opt.a = a
	opt.b = b
}

fn test_generics_return_multiple_generics_struct() {
	mut o1 := new_foo[int, string](23, 'aaa')
	println(get_a[int, string](o1))
	assert get_a[int, string](o1) == 23
	println(get_b[int, string](o1))
	assert get_b[int, string](o1) == 'aaa'
	set[int, string](mut o1, 42, 'bbb')
	println(get_a[int, string](o1))
	assert get_a[int, string](o1) == 42
	println(get_b[int, string](o1))
	assert get_b[int, string](o1) == 'bbb'

	mut o2 := new_foo[string, int]('bbb', 22)
	println(get_a[string, int](o2))
	assert get_a[string, int](o2) == 'bbb'
	println(get_b[string, int](o2))
	assert get_b[string, int](o2) == 22
	set[string, int](mut o2, 'bbb', 42)
	println(get_a[string, int](o2))
	assert get_a[string, int](o2) == 'bbb'
	println(get_b[string, int](o2))
	assert get_b[string, int](o2) == 42

	mut o3 := new_foo[int, bool](23, true)
	println(get_a[int, bool](o3))
	assert get_a[int, bool](o3) == 23
	println(get_b[int, bool](o3))
	assert get_b[int, bool](o3) == true
	set[int, bool](mut o3, 42, false)
	println(get_a[int, bool](o3))
	assert get_a[int, bool](o3) == 42
	println(get_b[int, bool](o3))
	assert get_b[int, bool](o3) == false
}
