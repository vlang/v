enum Foo {
	a
	b
	c
}

fn get() Foo {
	return .a
}

fn foo(f Foo) string {
	println(f)
	return '$f'
}

fn test_match_expr_with_enum() {
	ret := foo(match get() {
		.a { .b }
		.b { .c }
		.c { .a }
	})
	println(ret)
	assert ret == 'b'
}
