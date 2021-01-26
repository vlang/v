struct Options {
	a int
}

fn get_option(options Options) bool {
	if options.a == 1 {
		return true
	}
	return false
}

fn test_short_struct_as_parameter() {
	if get_option(Options{a: 1}) {
		assert true
		return
	}
	assert false
}
