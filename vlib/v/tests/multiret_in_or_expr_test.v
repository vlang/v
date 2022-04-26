fn multi_return() ?(int, int) {
	return 1, 2
}

fn test_multi_return_in_or_expr() {
	a, b := multi_return() or { 0, -1 }

	println('$a, $b')
	assert a == 1
	assert b == 2
}
