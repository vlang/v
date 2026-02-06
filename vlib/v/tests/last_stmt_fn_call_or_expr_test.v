module main

fn x1() ?int {
	return none
}

fn x2() ?int {
	return none
}

fn x3() ?int {
	return none
}

fn def() int {
	return 123
}

fn test_last_stmt_fn_call_or_expr() {
	y := x1() or { x2() or { x3() or { def() } } }
	assert y == 123
}
