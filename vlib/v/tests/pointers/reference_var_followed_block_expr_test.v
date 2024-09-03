fn test_reference_var_followed_block_expr() {
	mut b := [5, 6, 7]
	mut c := &b
	{
	}
	println(c)
	assert '${c}' == '&[5, 6, 7]'
}
