
struct Abc {
	x int
}

fn i_0(x int) ?int {
	if x == 0 {
		return error('my error 1')
	}
	return x
}

fn struct_0(x int) ?Abc {
	if x == 0 {
		return error('my error 2')
	}
	return Abc{x}
}

fn string_0(x int) ?string {
	if x == 0 {
		return error('my error 3')
	}
	return '$x'
}

fn return_a_string() string {
	return 'abcdef'
}

//

fn test_optional_int() {
	a := i_0(0) or {
		4999
	}
	assert a == 4999
	b := i_0(4123) or {
		4999
	}
	assert b == 4123
}

fn test_optional_struct() {
	sa := struct_0(0) or {
		Abc{7999}
	}
	assert sa.x == 7999
	sb := struct_0(3456) or {
		Abc{7999}
	}
	assert sb.x == 3456
}

fn test_optional_with_statements_before_last_expression() {
	s := struct_0(0) or {
		eprintln('hello')
		Abc{12345}
	}
	assert s.x == 12345
}

fn test_optional_with_fn_call_as_last_expression() {
	s := string_0(0) or {
		return_a_string()
	}
	assert s == 'abcdef'
}

fn test_optional_with_fn_call_last_expr_and_preceding_statements() {
	s := string_0(0) or {
		eprintln('hello')
		println('world')
		return_a_string()
	}
	assert s == 'abcdef'
}
