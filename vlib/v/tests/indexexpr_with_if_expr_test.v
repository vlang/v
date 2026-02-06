fn test_indexexpr_with_if_expr() {
	mut array_a := [1, 2, 3]
	mut array_b := [4, 5, 6]

	(if true {
		array_a
	} else {
		array_b
	})[0] = 999
	println(array_a)
	assert array_a == [999, 2, 3]
}
