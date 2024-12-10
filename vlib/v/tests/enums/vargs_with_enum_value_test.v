enum Foo {
	a
	b
	c
}

fn foo(n int, m ...map[Foo]int) {
	println(m)
	assert m.len == 2
	assert '${m}' == '[{a: 1}, {b: 2}]'
}

fn test_vargs_with_enum_value() {
	foo(1, {
		.a: 1
	}, {
		.b: 2
	})
}
