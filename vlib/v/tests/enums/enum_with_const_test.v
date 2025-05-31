enum Foo {
	a = c
	b = 1
	c = 2
}

const c = 0

fn test_enum_with_const() {
	mut foo := Foo.c
	foo = .a
	ret := match foo {
		.a { 'a' }
		.b { 'b' }
		.c { 'c' }
	}
	println(ret)
	assert ret == 'a'
}
