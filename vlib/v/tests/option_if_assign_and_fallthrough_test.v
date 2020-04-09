fn err_call(ok bool) ?int {
	if ok {
		return 42
	}
	return error('Not ok!')
}

fn test_if_opt() {
	if val := err_call(true) {
		eprintln('  val should be available here: $val')
		assert val == 42
	}
	assert true
}

fn test_opt_with_fall_through() {
	mut x := 1
	err_call(false) or {
		eprintln('  this *should* be an error: $err')
		x++
		assert true
	}
	assert x == 2
}
