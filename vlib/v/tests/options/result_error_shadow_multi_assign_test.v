fn failing_error_payload() !IError {
	return error('outer failure')
}

fn recovered_error_payload() !IError {
	failing_error_payload() or {
		return unsafe {
			_, err := 1, error('payload')
			err
		}
	}
	return error('unreachable')
}

fn test_multi_assignment_err_shadow_is_a_successful_result_payload() {
	value := recovered_error_payload() or { panic('unexpected failure: ${err}') }
	assert value.msg() == 'payload'
}
