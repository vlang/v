struct TOptions {
	a int
}

fn t(options TOptions) bool {
	if options.a == 1 {
		return true
	}
	return false
}

fn test_short_struct_as_parameter() {
	if t(a: 1) {
		assert true
		return
	}
	assert false
}
