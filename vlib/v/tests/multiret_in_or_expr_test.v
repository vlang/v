fn multi_return1() ?(int, int) {
	return 1, 2
}

fn multi_return2() ?(i64, i64) {
	return 11, 22
}

fn multi_return3() ?(int, i64) {
	return 11, 22
}

fn test_multi_return_in_or_expr() {
	a1, b1 := multi_return1() or { 0, -1 }

	println('${a1}, ${b1}')
	assert a1 == 1
	assert b1 == 2

	a2, b2 := multi_return2() or { 0, -1 }

	println('${a2}, ${b2}')
	assert a2 == 11
	assert b2 == 22

	a3, b3 := multi_return3() or { 0, -1 }

	println('${a3}, ${b3}')
	assert a3 == 11
	assert b3 == 22
}
