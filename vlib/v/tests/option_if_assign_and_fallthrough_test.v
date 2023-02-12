fn err_call(ok bool) ?int {
	if ok {
		return 42
	}
	return error('Not ok!')
}

fn test_if_opt() {
	if val := err_call(true) {
		eprintln('  val should be available here: ${val}')
		assert val == 42
	}
	assert true
}

fn test_simple_else_if_guard() {
	if false {
		assert false
	} else if val := err_call(true) {
		assert val == 42
	} else {
		assert false
	}
}

fn test_multiple_else_if_guard() {
	if _ := err_call(false) {
		assert false
	} else if val := err_call(false) {
		assert val == 0 // assert false
	} else if val := err_call(true) {
		assert val == 42
	} else {
		assert false
	}
}

fn test_opt_with_fall_through() {
	mut x := 1
	err_call(false) or {
		eprintln('  this *should* be an error: ${err}')
		x++
		assert true
	}
	assert x == 2
}
