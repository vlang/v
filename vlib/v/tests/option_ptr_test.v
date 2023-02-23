fn test_simple_opt_ptr() {
	val := 123
	mut var := unsafe { ?&int(&val) }
	assert var? == unsafe { &int(&val) }
}

fn test_simple_writing() {
	val := 123
	mut var := unsafe { ?&int(&val) }
	unsafe {
		*var? = 321
	}
	assert val == 321
}

fn test_simple_writing2() {
	val := 123
	mut var := unsafe { ?&int(&val) }
	// unsafe { var = 0 } // error
	unsafe {
		*var? = 321
	}
	x := *var?
	assert val == x
}
