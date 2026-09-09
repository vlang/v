fn result_ok(value string) !string {
	return value
}

fn result_error(message string) !string {
	return error(message)
}

fn test_or_block_with_err_source_fallback_not_executed() {
	err := ?string('test')
	got := err or { 'fallback' }

	assert got == 'test'
}

fn test_or_block_with_err_source_fallback_executed() {
	err := ?string(none)
	got := err or { 'fallback' }

	assert got == 'fallback'
}

fn test_or_block_err_value_keeps_special_meaning() {
	got := result_error('boom') or { err.msg() }

	assert got == 'boom'
}

fn test_or_block_err_source_reusable_after_block() {
	err := ?string(none)
	got := err or { 'fallback' }
	again := err or { 'again' }

	assert got == 'fallback'
	assert again == 'again'
}

fn test_nested_or_block_err_shadowing() {
	got := result_error('outer') or {
		inner_msg := result_error('inner') or { err.msg() }
		assert inner_msg == 'inner'
		assert err.msg() == 'outer'
		err.msg()
	}

	assert got == 'outer'
}

fn test_if_guard_inside_or_block_uses_guard_err() {
	got := result_error('outer') or {
		if value := result_error('guard') {
			value
		} else {
			err.msg()
		}
	}

	assert got == 'guard'
}

fn test_if_guard_with_option_source_named_err() {
	err := ?string('value')
	mut got := ''
	if value := err {
		got = value
	} else {
		got = 'fallback'
	}

	assert got == 'value'
}

fn test_map_index_or_block_on_value_path() {
	mut values := map[string]?string{}
	values['present'] = ?string('value')

	missing := values['missing'] or { 'fallback' }
	present := values['present'] or { 'fallback' }

	assert missing == 'fallback'
	assert present == 'value'
}

fn test_result_call_or_block_fallback_not_executed() {
	got := result_ok('ok') or { 'fallback' }

	assert got == 'ok'
}
