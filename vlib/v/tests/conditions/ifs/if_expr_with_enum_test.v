enum Foo {
	a
	b
}

fn get() Foo {
	return .a
}

fn foo(f Foo) string {
	println(f)
	return '${f}'
}

fn test_if_expr_with_enum_value() {
	ret := foo(if get() == .a { .b } else { .a })
	println(ret)
	assert ret == 'b'
}
