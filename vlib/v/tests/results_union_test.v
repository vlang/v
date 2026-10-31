struct ResultDetail {
	message string
	number  int
}

fn (e ResultDetail) msg() string {
	return e.message
}

fn (e ResultDetail) code() int {
	return e.number
}

enum ResultChoice {
	first = 3
	last  = 7
}

fn result_value[T](value T, fail bool) !T {
	if fail {
		return error_with_code('failed', 29)
	}
	return value
}

fn result_forward[T](value T, fail bool) !T {
	return result_value(value, fail)
}

fn result_array(value [4]u64, fail bool) ![4]u64 {
	return result_forward(value, fail)
}

fn result_detail(number int) !u64 {
	detail := ResultDetail{'detail ${number}', number}
	return detail
}

fn result_enum_error() !u64 {
	ResultChoice.from(5) or { return err }
	return 0
}

fn result_error_value(fail bool) !IError {
	if fail {
		return error_with_code('failure', 41)
	}
	return IError(ResultDetail{'payload', 17})
}

fn test_result_union_scalar_and_fixed_array_forwarding() {
	assert result_forward(u64(123), false)! == 123
	expected := [u64(3), 5, 8, 13]!
	assert result_forward(expected, false)! == expected
	result_forward(expected, true) or {
		assert err.msg() == 'failed'
		assert err.code() == 29
		return
	}
	assert false
}

fn test_result_union_error_payload_is_a_successful_value() {
	value := result_error_value(false)!
	assert value is ResultDetail
	assert value.msg() == 'payload'
	assert value.code() == 17
	result_error_value(true) or {
		assert err.msg() == 'failure'
		assert err.code() == 41
		return
	}
	assert false
}

fn test_result_union_preserves_escaping_error_object() {
	result_detail(17) or {
		assert err is ResultDetail
		assert err.msg() == 'detail 17'
		assert err.code() == 17
		return
	}
	assert false
}

fn test_result_union_preserves_static_error_after_return() {
	assert ResultChoice.from(3)! == .first
	result_enum_error() or {
		assert err.msg() == 'invalid value'
		assert err.code() == 0
		return
	}
	assert false
}

fn test_result_union_thread_fixed_array_return() {
	expected := [u64(21), 34, 55, 89]!
	success := spawn result_array(expected, false)
	assert success.wait()! == expected
	failure := spawn result_array(expected, true)
	failure.wait() or {
		assert err.msg() == 'failed'
		assert err.code() == 29
		return
	}
	assert false
}

fn result_pointer(detail ResultDetail) ! {
	return &detail
}

fn result_empty_error() ! {
	return Error{}
}

fn test_result_union_empty_error_is_still_failure() {
	result_empty_error() or {
		assert err is Error
		assert err.msg() == ''
		assert err.code() == 0
		return
	}
	assert false
}

fn test_result_union_pointer_error_outlives_parameter() {
	detail := ResultDetail{'borrowed', 61}
	result_pointer(detail) or {
		assert err is ResultDetail
		assert err.msg() == 'borrowed'
		assert err.code() == 61
		return
	}
	assert false
}

fn test_result_union_collects_thread_array_payloads() {
	expected := [u64(21), 34, 55, 89]!
	mut workers := []thread ![4]u64{}
	workers << spawn result_array(expected, false)
	workers << spawn result_array(expected, false)
	values := workers.wait()!
	assert values.len == 2
	assert values[0] == expected
	assert values[1] == expected
}

fn test_result_union_thread_array_preserves_error() {
	expected := [u64(21), 34, 55, 89]!
	mut workers := []thread ![4]u64{}
	workers << spawn result_array(expected, false)
	workers << spawn result_array(expected, true)
	workers << spawn result_array(expected, false)
	workers.wait() or {
		assert err.msg() == 'failed'
		assert err.code() == 29
		return
	}
	assert false
}
