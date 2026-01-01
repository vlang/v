import x.json2 as json

// Tests for strict mode: quoted strings are NOT decoded as numbers (JSON spec compliance).
// Tests for default mode: quoted strings ARE accepted as numbers for compatibility.

// ===== STRICT MODE TESTS =====

fn test_strict_string_not_decoded_as_int() {
	json.decode[int]('"0"', strict: true) or {
		assert err is json.JsonDecodeError
		if err is json.JsonDecodeError {
			assert err.message == 'Data: Expected number, but got string'
		}
		return
	}
	assert false, 'Expected JsonDecodeError for quoted number string in strict mode'
}

fn test_strict_string_not_decoded_as_int_positive() {
	json.decode[int]('"100"', strict: true) or {
		assert err is json.JsonDecodeError
		if err is json.JsonDecodeError {
			assert err.message == 'Data: Expected number, but got string'
		}
		return
	}
	assert false, 'Expected JsonDecodeError for quoted number string in strict mode'
}

fn test_strict_string_not_decoded_as_float() {
	json.decode[f64]('"-23.6e1"', strict: true) or {
		assert err is json.JsonDecodeError
		if err is json.JsonDecodeError {
			assert err.message == 'Data: Expected number, but got string'
		}
		return
	}
	assert false, 'Expected JsonDecodeError for quoted number string in strict mode'
}

fn test_strict_string_in_array_not_decoded_as_int() {
	json.decode[[]int]('["100", 99, "98", 97]', strict: true) or {
		assert err is json.JsonDecodeError
		if err is json.JsonDecodeError {
			assert err.message == 'Data: Expected number, but got string'
		}
		return
	}
	assert false, 'Expected JsonDecodeError for quoted number string in array in strict mode'
}

// ===== DEFAULT MODE TESTS =====

fn test_default_string_decoded_as_int() {
	assert json.decode[int]('"0"')! == 0
	assert json.decode[int]('"100"')! == 100
	assert json.decode[int]('"-50"')! == -50
}

fn test_default_string_decoded_as_float() {
	assert json.decode[f64]('"-23.6e1"')! == -236.0
	assert json.decode[f64]('"3.14"')! == 3.14
}

fn test_default_string_in_array_decoded_as_int() {
	assert json.decode[[]int]('["100", "99", "98", "97"]')! == [100, 99, 98, 97]
	assert json.decode[[]int]('["100", 99, "98", 97]')! == [100, 99, 98, 97]
}

// ===== COMMON TESTS (both modes) =====

fn test_valid_unquoted_numbers() {
	assert json.decode[int]('0')! == 0
	assert json.decode[int]('100')! == 100
	assert json.decode[int]('-50')! == -50
	assert json.decode[f64]('-23.6e1')! == -236.0
	assert json.decode[[]int]('[100, 99, 98, 97]')! == [100, 99, 98, 97]

	// Strict mode also works for unquoted numbers
	assert json.decode[int]('0', strict: true)! == 0
	assert json.decode[int]('100', strict: true)! == 100
}

fn test_invalid_number_string_fails() {
	json.decode[int]('"not_a_number"') or { return }
	assert false, 'Expected error for invalid number string'
}

fn test_leading_plus_in_string() {
	assert json.decode[int]('"+100"')! == 100

	json.decode[int]('"+100"', strict: true) or {
		assert err is json.JsonDecodeError
		return
	}
	assert false, 'Expected error in strict mode'
}
