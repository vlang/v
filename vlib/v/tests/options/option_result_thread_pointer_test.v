fn maybe_thread_pointer() ?&thread int {
	return none
}

fn required_thread_pointer() !&thread int {
	return error('not started')
}

fn test_option_pointer_to_a_thread_handle_can_be_none() {
	if _ := maybe_thread_pointer() {
		assert false
	}
}

fn test_result_pointer_to_a_thread_handle_preserves_the_error() {
	required_thread_pointer() or {
		assert err.msg() == 'not started'
		return
	}
	assert false
}
