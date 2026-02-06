type BOOL = bool

fn example() BOOL {
	return true
}

fn test_if_cond_with_alias() {
	if example() {
		println('Should work, or not?')
		assert true
	} else {
		assert false
	}
}
