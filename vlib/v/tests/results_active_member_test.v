type ActiveValue = int | string

fn active_value() (int, !ActiveValue) {
	return 7, ActiveValue(42)
}

struct ActiveSource {
mut:
	calls int
}

fn (mut source ActiveSource) text(fail bool) !string {
	source.calls++
	if fail {
		return error_with_code('missing', 37)
	}
	return 'present'
}

fn test_result_sum_assignment_preserves_both_states() {
	for fail in [false, true] {
		mut source := ActiveSource{}
		_, mut result := active_value()
		result = source.text(fail)
		assert source.calls == 1
		if value := result {
			assert !fail
			assert value is string
			assert value as string == 'present'
		} else {
			assert fail
			assert err.msg() == 'missing'
			assert err.code() == 37
		}
	}
}

fn active_text(mut source ActiveSource, fail bool) (int, !string) {
	return 7, source.text(fail)
}

fn active_text_pointer() (int, !&string) {
	mut initial := 'initial'
	return 7, &initial
}

fn test_result_pointer_assignment_preserves_both_states() {
	for fail in [false, true] {
		mut source := ActiveSource{}
		_, mut value := active_text(mut source, fail)
		_, mut result := active_text_pointer()
		result = &value
		if pointer := result {
			assert !fail
			assert *pointer == 'present'
		} else {
			assert fail
			assert err.msg() == 'missing'
			assert err.code() == 37
		}
	}
}

fn test_option_pointer_conversion_preserves_both_states() {
	pointer := active_pointer(true)?
	assert *pointer == 'present'
	assert active_pointer(false) == none
}

@[noinline]
fn active_pointer(present bool) ?&string {
	value := if present { ?string('present') } else { ?string(none) }
	return ?&string(value)
}

fn test_option_sum_conversion_preserves_both_states() {
	mut result := ?ActiveValue(none)
	result = ?string('present')
	value := result?
	assert value is string
	assert value as string == 'present'
	result = ?string(none)
	assert result == none
}
