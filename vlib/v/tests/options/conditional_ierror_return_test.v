import io

fn conditional_number(eof bool) !int {
	return if eof { IError(io.Eof{}) } else { 42 }
}

fn matched_number(choice int) !int {
	return match choice {
		0 { IError(io.Eof{}) }
		1 { IError(io.Eof{}) }
		else { 42 }
	}
}

fn conditional_error_payload(eof bool) !IError {
	return if eof { IError(io.Eof{}) } else { IError(io.NotExpected{}) }
}

fn nested_conditional_number(outer bool, inner bool) !int {
	return if outer {
		if inner { IError(io.Eof{}) } else { 11 }
	} else {
		22
	}
}

fn nested_match_number(outer bool, choice int) !int {
	return if outer {
		match choice {
			0 { IError(io.Eof{}) }
			else { 11 }
		}
	} else {
		22
	}
}

fn test_conditional_ierror_is_result_failure() {
	assert conditional_number(false)! == 42
	if _ := conditional_number(true) {
		assert false
	} else {
		assert err is io.Eof
	}
	for choice in 0 .. 2 {
		if _ := matched_number(choice) {
			assert false
		} else {
			assert err is io.Eof
		}
	}
	assert matched_number(2)! == 42
}

fn test_conditional_ierror_can_be_successful_payload() {
	first := conditional_error_payload(true)!
	second := conditional_error_payload(false)!
	assert first is io.Eof
	assert second is io.NotExpected
}

fn test_nested_conditional_ierror_is_result_failure() {
	assert nested_conditional_number(false, true)! == 22
	assert nested_conditional_number(true, false)! == 11
	if _ := nested_conditional_number(true, true) {
		assert false
	} else {
		assert err is io.Eof
	}
	assert nested_match_number(false, 0)! == 22
	assert nested_match_number(true, 1)! == 11
	if _ := nested_match_number(true, 0) {
		assert false
	} else {
		assert err is io.Eof
	}
}

struct ConditionalPayloadError {
	value int
}

fn (e ConditionalPayloadError) msg() string { return 'payload' }

fn (e ConditionalPayloadError) code() int { return e.value }

fn narrowed_error_result(item IError) !ConditionalPayloadError {
	return if item is ConditionalPayloadError { item } else { ConditionalPayloadError{ value: 2 } }
}

fn narrowed_error_option(item IError) ?ConditionalPayloadError {
	return if item is ConditionalPayloadError { item } else { ConditionalPayloadError{ value: 2 } }
}

fn negated_error_result(item IError) !ConditionalPayloadError {
	return if item !is ConditionalPayloadError { ConditionalPayloadError{ value: 2 } } else { item }
}

fn negated_error_option(item IError) ?ConditionalPayloadError {
	return if item !is ConditionalPayloadError { ConditionalPayloadError{ value: 2 } } else { item }
}

fn negated_error_else_if(item IError, keep bool) !ConditionalPayloadError {
	return if item !is ConditionalPayloadError {
		ConditionalPayloadError{ value: 2 }
	} else if keep {
		item
	} else {
		ConditionalPayloadError{ value: 3 }
	}
}

fn matched_error_result(item IError) !ConditionalPayloadError {
	return match item {
		ConditionalPayloadError { item }
		else { ConditionalPayloadError{ value: 2 } }
	}
}

fn matched_error_option(item IError) ?ConditionalPayloadError {
	return match item {
		ConditionalPayloadError { item }
		else { ConditionalPayloadError{ value: 2 } }
	}
}

fn test_smartcasted_ierror_is_successful_payload() {
	for item in [IError(ConditionalPayloadError{ value: 41 }), IError(io.Eof{})] {
		expected := if item is ConditionalPayloadError { 41 } else { 2 }
		assert narrowed_error_result(item)!.value == expected
		assert narrowed_error_option(item)?.value == expected
		assert negated_error_result(item)!.value == expected
		assert negated_error_option(item)?.value == expected
		assert negated_error_else_if(item, true)!.value == expected
		assert negated_error_else_if(item, false)!.value == if expected == 41 { 3 } else { 2 }
		assert matched_error_result(item)!.value == expected
		assert matched_error_option(item)?.value == expected
	}
}
