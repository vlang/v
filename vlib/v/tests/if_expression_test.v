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

fn get_bool_str(b bool) string {
	return b.str()
}

fn test_if_expression_mutate_var() {
	mut b := false
	r := b && if true {
		b = true
		true
	} else {
		true
	}
	assert r == false
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

fn test_if_expr_with_infix() {
	a := if true { 1 } else { 0 } + 5
	assert a == 6
}

fn test_multi_if_expr_with_infix() {
	a := if 1 == 0 { 1 } else if 1 == 0 { 2 } else { 3 } + 4
	assert a == 7
}
