fn test_if_expression_precedence_false_condition() {
	b := 10
	c := 20
	res := 1 + if b > c { b } else { c } + 1
	assert res == c + 2
}

fn test_if_expression_precedence_true_condition() {
	b := 20
	c := 10
	res := 1 + if b > c { b } else { c } + 1
	assert res == b + 2
}

fn test_if_expression_with_stmts() {
	a := if true {
		b := 1
		b
	} else {
		b := 4
		b
	}
	assert a == 1
	mut b := 0
	b = if false {
		42
	} else {
		24
	}
	assert b == 24
}

fn noop() {
}

fn test_if_expression_with_function_assign() {
	a := if true {
		my_fn := noop
		my_fn()
		0
	} else {
		1
	}
	assert a == 0
}

fn test_blank_ident() {
	_ := if true {
		assert true
		true
	} else {
		assert false
		false
	}
}

fn test_infix_expr() {
	/*
	a := 1 + if true {
		noop()
		2
	} else {
		3
	}
	assert a == 3
	assert 3.3 + if false {
		0.0
	} else {
		noop()
		1.1
	} == 4.4
	b := 'Hello ' + if false {
		noop()
		'Everybody'
	} else {
		'world'
	}
	assert b == 'Hello world'
	assert 'N' + if true {
		noop()
		'ah'
	} else {
		'!'
	} == 'Nah'
	*/
	/*
	// TODO Enable when fixed in checker
	c := if true {
		noop()
		1
	} else {
		2
	} + 3
	assert c == 4
	assert if true {
		noop()
		1
	} else {
		2
	} + 3 == 4
	d := if true {
		noop()
		1
	} else {
		2
	} - if false {
		noop()
		2
	} else {
		0
	}
	assert d == 1
	assert if true {
		noop()
		1
	} else {
		2
	} - if false {
		noop()
		2
	} else {
		0
	} == 1
	*/
}

fn get_bool_str(b bool) string {
	return b.str()
}

/*
fn test_if_expression_mutate_var() {
	mut b := false
	r := b && if true {
		b = true
		true
	} else {
		true
	}
	assert r == false
	assert 1 + if true {
		println('two')
		2
	} else {
		println('three')
		3
	} == 3
	// test in function call
	assert get_bool_str(b && if true {
		b = true
		true
	} else {
		true
	}) == 'false'
	// test on method call
	assert (b && if true {
		b = true
		true
	} else {
		true
	}).str() == 'false'
	// test on array
	mut a := [1, 2]
	assert a.len == 2 && if true {
		a << 3
		true
	} else {
		false
	}
	mut d := false
	_ := false && if true {
		d = true
		false
	} else {
		false
	}
	assert d == false
}
*/

fn test_loop_in_if_expr() {
	a := if true {
		mut i := 0
		for _ in 1 .. 3 {
			i++
		}
		i
	} else {
		3
	}
	assert a == 2
}

fn test_anon_fn_in_if_expr() {
	a := if true {
		f := fn (i int) int {
			return i + 3
		}
		g := f(6)
		g + 1
	} else {
		0
	}
	assert a == 10
	b := if true {
		f := fn (i int) int {
			return i + 3
		}
		f(7)
	} else {
		0
	}
	assert b == 10
}

fn ret_none() ?int {
	return none
}

fn test_opt_call_in_if_expr() {
	a := if true {
		b := ret_none() or {
			2
		}
		b
	} else {
		0
	}
	assert a == 2
}

fn test_opt_call_as_last_stmt_in_if_expr() {
	a := if false {
		0
	} else {
		ret_none() or {
			1
		}
	}
	assert a == 1
}

fn err_call(err bool) ?int {
	if err {
		return error('Nah')
	}
	return 0
}

fn propagate_opt(err bool) ?int {
	a := if true { err_call(err)? } else { 1 }
	return a
}

fn test_opt_propagate_in_if_expr() {
	propagate_opt(false) or {
		assert false
	}
	if _ := propagate_opt(true) {
		assert false
	}
}

fn test_nested_if_expressions() {
	a := if true { if true { 'a' } else { 'b' } } else { 'c' }
	assert a == 'a'
}

fn test_simple_nested_if_expressions() {
	a := if false {
		b := 1
		if b == 0 {
			0
		} else {
			b
		}
	} else {
		println('Hello world !')
		if 1 == 1 {
			t := 12
			t + 42
		} else {
			43
		}
	}
	assert a == 54
}

fn test_complex_nested_if_expressions() {
	mut a := false
	a = (1 == 2 || true) && (if true {
		g := 6
		h := if false { 3 } else { 5 }
		mut d := false
		if h == 2 {
			d = g + 4 == 5
		}
		for _ in 1 .. 3 {
			d = !d
		}
		if d {
			if true {
				d = false
			} else {
				d = true
			}
		}
		d
	} else {
		x := 6
		y := 8
		if x + y > 0 {
			x > 0
		} else {
			false
		}
	})
	assert a == false
}

fn test_lots_of_if_expressions() {
	mut a := 0
	if true {
		if true {
			if true {
				if true {
					if true {
						if true {
							if true {
								if true {
									if true {
										if true {
											if true {
												a = 1
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
	assert a == 1
}
