fn err_scope_result(present bool) !string {
	if present {
		return 'value'
	}
	return error('failure')
}

fn err_scope_option(present bool) ?string {
	if present {
		return 'option'
	}
	return none
}

fn err_scope_outer_result() !string {
	return error('outer failure')
}

fn err_scope_pair(first string, second string) (string, string) {
	return first, second
}

fn test_result_guard_assigns_outer_string_err() {
	for present in [false, true] {
		mut err, mut tag := 'outer', ''
		err, tag = if value := err_scope_result(present) {
			value, 'ok'
		} else {
			err.msg(), 'bad'
		}
		assert err == if present { 'value' } else { 'failure' }
		assert tag == if present { 'ok' } else { 'bad' }
	}
}

fn test_result_guard_assigns_outer_ierror_err() {
	mut err := error('outer')
	mut tag := ''
	err, tag = if value := err_scope_result(false) {
		error(value), 'ok'
	} else {
		err, 'bad'
	}
	assert err.msg() == 'failure'
	assert tag == 'bad'
}

fn test_result_guard_assigns_captured_outer_err() {
	mut err, mut tag := 'outer', ''
	ptr := &err
	read := fn [ptr] () string {
		return *ptr
	}
	err, tag = if value := err_scope_result(false) { value, 'ok' } else { err.msg(), 'bad' }
	assert read() == 'failure'
	assert err == 'failure'
	assert tag == 'bad'
}

fn test_result_guard_assigns_call_tail_outside_error_scope() {
	mut err, mut tag := 'outer', ''
	err, tag = if value := err_scope_result(false) {
		err_scope_pair(value, 'ok')
	} else {
		err_scope_pair(err.msg(), 'call')
	}
	assert err == 'failure'
	assert tag == 'call'
}

fn test_result_guard_assigns_nested_tail_outside_error_scope() {
	for chosen in [false, true] {
		mut err, mut tag := 'outer', ''
		err, tag = if value := err_scope_result(false) {
			value, 'ok'
		} else {
			if chosen { err.msg(), 'chosen' } else { 'fallback', 'other' }
		}
		assert err == if chosen { 'failure' } else { 'fallback' }
		assert tag == if chosen { 'chosen' } else { 'other' }
	}
}

fn test_result_guard_lvalues_use_outer_err_after_rhs() {
	mut err, mut tag := 'outer', ''
	mut values := {
		'outer': 'before'
	}
	values[err], tag = if value := err_scope_result(false) {
		value, 'ok'
	} else {
		err.msg(), 'bad'
	}
	assert values['outer'] == 'failure'
	assert err == 'outer'
	assert tag == 'bad'
	mut index := 0
	mut destinations := ['before', 'before']
	err, destinations[index] = if value := err_scope_result(false) {
		value, 'ok'
	} else {
		index = 1
		err.msg(), 'bad'
	}
	assert err == 'failure'
	assert destinations == ['before', 'bad']
}

fn test_result_guard_else_if_uses_outer_err() {
	for chosen in [false, true] {
		err := 'outer'
		first, tag := if value := err_scope_result(false) {
			value, 'ok'
		} else if chosen && err == 'outer' {
			err, 'chosen'
		} else {
			err, 'other'
		}
		assert first == 'outer'
		assert tag == if chosen { 'chosen' } else { 'other' }
	}
}

fn test_result_guard_else_if_assignment_uses_outer_err() {
	mut err, mut tag := 'outer', ''
	err, tag = if value := err_scope_result(false) {
		value, 'ok'
	} else if err == 'outer' {
		err + '-chosen', 'chosen'
	} else {
		err, 'other'
	}
	assert err == 'outer-chosen'
	assert tag == 'chosen'
}

fn test_result_guard_option_else_if_uses_outer_err() {
	for present in [false, true] {
		err := 'outer'
		first, tag := if value := err_scope_result(false) {
			value, 'result'
		} else if value := err_scope_option(present) {
			value, err
		} else {
			err, 'none'
		}
		assert first == if present { 'option' } else { 'outer' }
		assert tag == if present { 'outer' } else { 'none' }
	}
}

fn err_scope_return(chosen bool) (string, string) {
	err := 'outer'
	return if value := err_scope_result(false) {
		value, 'ok'
	} else if chosen && err == 'outer' {
		err, 'chosen'
	} else {
		err, 'other'
	}
}

fn test_return_result_guard_else_if_uses_outer_err() {
	for chosen in [false, true] {
		first, tag := err_scope_return(chosen)
		assert first == 'outer'
		assert tag == if chosen { 'chosen' } else { 'other' }
	}
}

fn test_result_guard_else_if_keeps_older_implicit_err() {
	err_scope_outer_result() or {
		first, tag := if value := err_scope_result(false) {
			value, 'ok'
		} else if err.msg() == 'outer failure' {
			err.msg(), 'outer error'
		} else {
			err.msg(), 'other'
		}
		assert first == 'outer failure'
		assert tag == 'outer error'
		''
	}
}
