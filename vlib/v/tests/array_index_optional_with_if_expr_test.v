fn test_array_index_optional_with_if_expr() {
	ret := []string{}[0] or {
		if true { 'a' } else { 'b' }
	}
	println(ret)
	assert ret == 'a'
}
