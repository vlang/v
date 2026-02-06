fn test_if_expr_with_continue_in_branch() {
	mut result := []int{}
	for i in 0 .. 10 {
		s := if i < 2 {
			100 + i
		} else {
			continue
		}

		println(s)
		result << s
	}
	println(result)
	assert result.len == 2
	assert result[0] == 100
	assert result[1] == 101
}
