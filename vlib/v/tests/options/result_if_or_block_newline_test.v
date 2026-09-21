fn maybe_result_value(ok bool) !int {
	if !ok {
		return error('failed')
	}
	return 42
}

fn handle_same_line_if_result(ok bool) bool {
	// vfmt off
	if ok {
		maybe_result_value(ok)
	} else {
		maybe_result_value(ok)
	} or { return true }
	// vfmt on
	return false
}

fn handle_next_line_if_result(ok bool) bool {
	// vfmt off
	if ok {
		maybe_result_value(ok)
	} else {
		maybe_result_value(ok)
	}
		or { return true }
	// vfmt on
	return false
}

fn test_result_valued_if_accepts_trailing_or_block() {
	assert !handle_same_line_if_result(true)
	assert handle_same_line_if_result(false)
	assert !handle_next_line_if_result(true)
	assert handle_next_line_if_result(false)
}
