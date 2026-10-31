type MaybeNumber = ?i64

struct OptionalRecord {
mut:
	number MaybeNumber
	name   ?string
}

fn optional_value[T](value T, present bool) ?T {
	if !present {
		return none
	}
	return value
}

fn forward_optional(value i64, present bool) ?i64 {
	result := optional_value(value, present)?
	return result
}

fn optional_pair(present bool) ?(i64, string) {
	if !present {
		return none
	}
	return 42, 'answer'
}

fn failing_result() !i64 {
	return error_with_code('failure', 27)
}

fn forwarded_result() !i64 {
	return failing_result()!
}

fn test_option_value_lifecycle() {
	mut record := OptionalRecord{}
	assert record.number == none
	record.number = forward_optional(42, true)
	assert record.number? == 42
	record.number = forward_optional(42, false)
	assert record.number == none
	record.name = optional_value('answer', true)
	assert record.name? == 'answer'
	values := [optional_value(i64(42), true), optional_value(i64(0), false)]
	first := values[0]
	assert first? == 42
	assert values[1] == none
	fixed := optional_value([i64(1), 2, 3]!, true)?
	assert fixed == [i64(1), 2, 3]!
	left, right := optional_pair(true)?
	assert left == 42
	assert right == 'answer'
	missing_left, missing_right := optional_pair(false) or { i64(0), 'missing' }
	assert missing_left == 0
	assert missing_right == 'missing'
}

fn test_option_failure_preserves_outer_binding() {
	err := 'outer'
	value := optional_value(42, false) or {
		assert err == 'outer'
		7
	}
	assert value == 7
	if _ := optional_value(42, false) {
		assert false
	} else {
		assert err == 'outer'
	}
}

fn test_option_handler_can_declare_err() {
	local := optional_value(42, false) or {
		err := 19
		err
	}
	assert local == 19
}

fn test_result_error_survives_propagation() {
	value := forwarded_result() or {
		assert err.msg() == 'failure'
		assert err.code() == 27
		optional_value(i64(7), false) or {
			assert err.code() == 27
			i64(9)
		}
	}
	assert value == 9
	if _ := forwarded_result() {
		assert false
	} else {
		assert err.msg() == 'failure'
		assert err.code() == 27
	}
}

struct ErrorNamedPayload {
	ok  bool
	err string
}

fn test_option_keeps_payload_fields_named_like_result_fields() {
	payload := ErrorNamedPayload{ err: 'data' }
	wrapped := optional_value(payload, true)
	unwrapped := wrapped or { panic('missing payload') }
	assert !unwrapped.ok
	assert unwrapped.err == 'data'
}
