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
