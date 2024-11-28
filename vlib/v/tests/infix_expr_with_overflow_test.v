fn test_infix_expr_with_overflow() {
	a := u8(255)
	b := u8(1)
	c := a + b

	println(c)
	assert c == 0

	println(a + b)
	assert a + b == 0

	println(a + b < a)
	assert a + b < a
}
