struct Foo<A, B> {
	a A
	b B
}

fn (num Foo<A, B>) get_foo1() (A, B) {
	return num.a, num.b
}

fn (num Foo<A, B>) get_foo2<B, A>() (A, B) {
	return num.a, num.b
}

fn test_generics_with_multi_generics_struct_receiver() {
	num := Foo<int, string>{
		a: 3
		b: 'aaa'
	}
	a1, b1 := num.get_foo1()
	println('${a1}, ${b1}')
	assert a1 == 3
	assert b1 == 'aaa'

	a2, b2 := num.get_foo2()
	println('${a2}, ${b2}')
	assert a2 == 3
	assert b2 == 'aaa'
}
