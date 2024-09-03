fn test_match_branch_with_array_expression() {
	ret := match true {
		[''].contains('') { 22 }
		else { 0 }
	}
	println(ret)
	assert ret == 22
}
