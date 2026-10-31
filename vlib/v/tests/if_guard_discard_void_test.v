fn void_result(ok bool) ! {
	if !ok {
		return error('no')
	}
}

fn void_option(ok bool) ? {
	if !ok {
		return none
	}
}

fn int_option(ok bool) ?int {
	return if ok { 7 } else { none }
}

fn test_if_guard_discard_void_result_as_value() {
	ok := if _ := void_result(true) { 'ok' } else { 'err: ${err}' }
	assert ok == 'ok'
	failed := if _ := void_result(false) { 'ok' } else { 'err: ${err}' }
	assert failed == 'err: no'
}

fn test_if_guard_discard_void_option_as_value() {
	ok := if _ := void_option(true) { 'ok' } else { 'none' }
	assert ok == 'ok'
	failed := if _ := void_option(false) { 'ok' } else { 'none' }
	assert failed == 'none'
}

fn test_if_guard_discard_valued_option_as_value() {
	ok := if _ := int_option(true) { 'ok' } else { 'none' }
	assert ok == 'ok'
}

fn test_if_guard_discard_void_statement() {
	mut s := ''
	if _ := void_result(false) {
		s = 'ok'
	} else {
		s = err.msg()
	}
	assert s == 'no'
	if _ := void_option(true) {
		s = 'ok'
	} else {
		s = 'none'
	}
	assert s == 'ok'
}
