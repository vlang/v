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

fn test_match_expr_infers_parenthesized_enum_shorthands() {
	for i, expected in [Foo.a, .b, .c] {
		value := match i {
			0 { Foo.a }
			1 { (.b) }
			else { (.c) }
		}
		assert value == expected
	}
}

@[flag]
enum MatchAccess {
	read
	write
	execute
}

fn test_match_expr_infers_flag_enum_expressions() {
	expected_values := [MatchAccess.read, MatchAccess.read | .write, MatchAccess.write,
		MatchAccess.read ^ .execute, MatchAccess.read | .write]
	for i, expected in expected_values {
		value := match i {
			0 { MatchAccess.read }
			1 { .read | .write }
			2 { (.read | .write) & .write }
			3 { .read ^ .execute }
			else { (.read | (.write)) }
		}
		assert value == expected
	}
}

type FooOrInt = Foo | int

fn test_match_expr_enum_inference_preserves_branch_smartcast() {
	for i, input in [FooOrInt(0), FooOrInt(Foo.b)] {
		value := match input {
			int { Foo.a }
			Foo { input }
		}
		assert value == [Foo.a, .b][i]
		reversed := match input {
			Foo { input }
			int { (.a) }
		}
		assert reversed == value
	}
}

fn test_match_expr_enum_inference_uses_branch_local_types() {
	for i in 0 .. 2 {
		value := match i {
			0 {
				access := MatchAccess.read
				access
			}
			else {
				access := MatchAccess.write
				(.read | access)
			}
		}
		assert value == [MatchAccess.read, MatchAccess.read | .write][i]
	}
}
