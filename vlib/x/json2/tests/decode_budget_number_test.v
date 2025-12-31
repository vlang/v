import x.json2 as json

// Test that quoted strings are NOT decoded as numbers (JSON spec compliance).
// According to JSON spec, numbers must be unquoted.

fn test_string_not_decoded_as_int() {
	// '"0"' is a JSON string, not a number - should fail
	json.decode[int]('"0"') or {
		assert err is json.JsonDecodeError
		if err is json.JsonDecodeError {
			assert err.message == 'Data: Expected number, but got string'
		}
		return
	}
	assert false, 'Expected JsonDecodeError for quoted number string'
}

fn test_string_not_decoded_as_int_positive() {
	// '"100"' is a JSON string, not a number - should fail
	json.decode[int]('"100"') or {
		assert err is json.JsonDecodeError
		if err is json.JsonDecodeError {
			assert err.message == 'Data: Expected number, but got string'
		}
		return
	}
	assert false, 'Expected JsonDecodeError for quoted number string'
}

fn test_string_not_decoded_as_float() {
	// '"-23.6e1"' is a JSON string, not a number - should fail
	json.decode[f64]('"-23.6e1"') or {
		assert err is json.JsonDecodeError
		if err is json.JsonDecodeError {
			assert err.message == 'Data: Expected number, but got string'
		}
		return
	}
	assert false, 'Expected JsonDecodeError for quoted number string'
}

fn test_string_in_array_not_decoded_as_int() {
	// Array with mixed quoted and unquoted numbers - quoted ones should fail
	json.decode[[]int]('["100", 99, "98", 97]') or {
		assert err is json.JsonDecodeError
		if err is json.JsonDecodeError {
			assert err.message == 'Data: Expected number, but got string'
		}
		return
	}
	assert false, 'Expected JsonDecodeError for quoted number string in array'
}

fn test_valid_unquoted_numbers() {
	// Unquoted numbers should still work correctly
	assert json.decode[int]('0')! == 0
	assert json.decode[int]('100')! == 100
	assert json.decode[int]('-50')! == -50
	assert json.decode[f64]('-23.6e1')! == -236.0
	assert json.decode[[]int]('[100, 99, 98, 97]')! == [100, 99, 98, 97]
}

fn test_budget_number_malformed() {
	json.decode[int]('"+100"') or {
		if err is json.JsonDecodeError {
			// Now that we reject strings as numbers, the error is about type mismatch
			assert err.message == 'Data: Expected number, but got string'
		}
		return
	}
	assert false, 'Expected JsonDecodeError'
}
