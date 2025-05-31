fn test_match_with_mut_cond_var() {
	mut aaa := []string{}
	for mut ccc in aaa {
		match ccc {
			'/' {}
			else {}
		}
	}
	assert true
}
