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

fn b_0(b bool) ?bool {
	if b == false {
		return error('my error 4')
	}
	return b
}

fn return_a_string() string {
	return 'abcdef'
}

//
fn test_optional_int() {
	a := i_0(0) or { 4999 }
	assert a == 4999
	b := i_0(4123) or { 4999 }
	assert b == 4123
}

/*
fn test_optional_bool() {
	a := true && b_0(false) {
		true
	}
	assert a == true
	b := false || b_0(true) {
		false
	}
	assert b == true
}
*/
fn test_optional_struct() {
	sa := struct_0(0) or { Abc{7999} }
	assert sa.x == 7999
	sb := struct_0(3456) or { Abc{7999} }
	assert sb.x == 3456
}

fn test_optional_with_statements_before_last_expression() {
	s := struct_0(0) or {
		eprintln('hello')
		Abc{12345}
	}
	assert s.x == 12345
	b := b_0(true) or { false }
	assert b == true
}

fn test_optional_with_fn_call_as_last_expression() {
	s := string_0(0) or { return_a_string() }
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

fn test_nested_optional() {
	a := i_0(1) or {
		b := i_0(0) or { 3 }
		b
	}
	assert a == 1
	b := i_0(0) or {
		c := i_0(1) or { 3 }
		c
	}
	assert b == 1
	c := i_0(0) or {
		d := i_0(0) or { 3 }
		d
	}
	assert c == 3
}

fn test_nested_optional_with_opt_fn_call_as_last_value() {
	a := i_0(1) or { i_0(0) or { 3 } }
	assert a == 1
	b := i_0(0) or { i_0(1) or { 3 } }
	assert b == 1
	c := i_0(0) or { i_0(0) or { 3 } }
	assert c == 3
	// TODO Enable once optional in boolean expressions are working
	// d := b_0(true) or {
	// 	false && b_0(true) or {
	// 		true
	// 	}
	// }
	// assert d == false
	// e := b_0(true) or {
	// 	true && b_0(true) or {
	// 		false
	// 	}
	// }
	// assert e == false
}

fn remove_suffix1(s string) string {
	n := s.len
	i := s.last_index('.') or { n }
	return s[0..i]
}

fn test_var_inside_or_block() {
	assert remove_suffix1('Vlang.foo') == 'Vlang'
}
