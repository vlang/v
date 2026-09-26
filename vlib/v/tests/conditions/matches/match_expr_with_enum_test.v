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
	return '${f}'
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

fn match_enum_value(name string) !Foo {
	value := match name {
		'a' { Foo.a }
		'b' { .b }
		'c' { .c }
		else { return error('unknown value') }
	}
	return value
}

fn test_match_expr_infers_enum_shorthands_from_first_branch() {
	assert match_enum_value('a')! == .a
	assert match_enum_value('b')! == .b
	assert match_enum_value('c')! == .c
	if value := match_enum_value('unknown') {
		assert false, 'unexpected value: ${value}'
	} else {
		assert err.msg() == 'unknown value'
	}
}

fn test_match_expr_infers_enum_shorthand_in_else_branch() {
	for i, expected in [Foo.a, .b, .c] {
		value := match i {
			0 { Foo.a }
			1 { .b }
			else { .c }
		}
		assert value == expected
	}
}
