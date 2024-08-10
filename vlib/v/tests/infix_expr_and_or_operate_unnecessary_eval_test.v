fn f(s string) !int {
	if s == '' {
		return error('invalid s')
	}
	return s.len
}

fn test_infix_expr_and_or_operate_unnecessary_eval() {
	v := ''
	x := v != 'xyz' || f(v)! < f('abc')!
	dump(x)
	assert x

	y := v == 'xyz' && f(v)! < f('abc')!
	dump(y)
	assert !y
}
