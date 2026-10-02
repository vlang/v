fn cleanup_result_void(fails bool, mut events []string) ! {
	events << 'result called'
	if fails {
		return error('cleanup failed')
	}
}

fn cleanup_option_void(absent bool, mut events []string) ? {
	events << 'option called'
	if absent {
		return none
	}
}

fn late_void_cleanup(fails bool, absent bool, mut events []string) {
	defer {
		cleanup_result_void(fails, mut events) or { events << 'error: ${err}' }
		events << 'result done'
		cleanup_option_void(absent, mut events) or { events << 'none' }
		events << 'option done'
	}
	events << 'late body'
}

fn generic_void_cleanup[T](value T, fails bool, absent bool, mut events []string) {
	defer {
		late_void_cleanup(fails, absent, mut events)
	}
	events << 'generic: ${value}'
}

fn test_deferred_generic_void_result_and_option_success() {
	mut events := []string{}
	generic_void_cleanup(42, false, false, mut events)
	assert events == ['generic: 42', 'late body', 'result called', 'result done', 'option called',
		'option done']
}

fn test_deferred_generic_void_result_and_option_failure() {
	mut events := []string{}
	generic_void_cleanup('test', true, true, mut events)
	assert events == ['generic: test', 'late body', 'result called', 'error: cleanup failed',
		'result done', 'option called', 'none', 'option done']
}
