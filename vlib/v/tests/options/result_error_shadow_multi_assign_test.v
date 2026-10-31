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

struct ConcreteShadowError {
	detail string
}

fn (err ConcreteShadowError) msg() string {
	return err.detail
}

fn (err ConcreteShadowError) code() int {
	return 1
}

fn failing_concrete_error_payload() !ConcreteShadowError {
	return error('outer failure')
}

fn recovered_concrete_error_payload() !ConcreteShadowError {
	failing_concrete_error_payload() or {
		return unsafe {
			err := ConcreteShadowError{ detail: 'concrete payload' }
			err
		}
	}
	return ConcreteShadowError{ detail: 'unreachable' }
}

fn test_concrete_err_shadow_is_a_successful_result_payload() {
	value := recovered_concrete_error_payload() or { panic('unexpected failure: ${err}') }
	assert value.msg() == 'concrete payload'
}

fn recovered_multi_concrete_error_payload() !ConcreteShadowError {
	failing_concrete_error_payload() or {
		return unsafe {
			_, err := 1, ConcreteShadowError{ detail: 'multi payload' }
			err
		}
	}
	return ConcreteShadowError{ detail: 'unreachable' }
}

fn recovered_non_error_payload() !int {
	failing_concrete_error_payload() or {
		return unsafe {
			err := 42
			err
		}
	}
	return 0
}

fn test_shadowed_err_type_is_used_for_every_payload_binding() {
	value := recovered_multi_concrete_error_payload() or { panic('unexpected failure: ${err}') }
	assert value.msg() == 'multi payload'
	assert recovered_non_error_payload() or { panic('unexpected failure: ${err}') } == 42
}
