fn number() int|none {
	return none
}

fn test_match_sumtype_var_with_none() {
	n := number()
	ret := match n {
		int { 'n: $n' }
		none { '?' }
	}
	println(ret)
	assert ret == '?'
}
