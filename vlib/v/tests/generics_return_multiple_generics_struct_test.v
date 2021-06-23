struct Foo<A, B> {
mut:
	a A
	b B
}

fn new_foo<A, B>(a A, b B) Foo<A, B> {
	return {
		a: a
		b: b
	}
}

fn get_a<A, B>(opt Foo<A, B>) A {
	return opt.a
}

fn get_b<A, B>(opt Foo<A, B>) B {
	return opt.b
}

fn set<A, B>(mut opt Foo<A, B>, a A, b B) {
	opt.a = a
	opt.b = b
}

fn test_generics_return_multiple_generics_struct() {
	mut o := new_foo<int, string>(23, 'aaa')
	println(get_a<int, string>(o))
	assert get_a<int, string>(o) == 23
	println(get_b<int, string>(o))
	assert get_b<int, string>(o) == 'aaa'
	set<int, string>(mut o, 42, 'bbb')
	println(get_a<int, string>(o))
	assert get_a<int, string>(o) == 42
	println(get_b<int, string>(o))
	assert get_b<int, string>(o) == 'bbb'
}
