interface Any {}

fn return_any(val Any) ?Any {
	return val
}

fn test_cast_int_to_interface() {
	code := 200
	if an := return_any(code) {
		if an is int {
			println('an is an int!')
		} else {
			println('an is not an int!')
		}
		assert '${an}' == 'Any(200)'
	} else {
		assert false
	}
}
