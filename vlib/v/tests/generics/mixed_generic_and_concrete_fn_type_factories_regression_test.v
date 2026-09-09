struct Issue18077FieldError {
}

type Issue18077Validator[T] = fn (data T) ?Issue18077FieldError

fn issue18077_get_max_validator[T](max_value T) Issue18077Validator[T] {
	_ = max_value
	return fn [T](data T) ?Issue18077FieldError {
		_ = data
		return none
	}
}

fn issue18077_get_max_length_validator(length int) Issue18077Validator[string] {
	_ = length
	return fn (data string) ?Issue18077FieldError {
		_ = data
		return Issue18077FieldError{}
	}
}

const issue18077_validate_int = issue18077_get_max_validator[int](10)

fn test_issue_18077_mixed_generic_and_concrete_fn_type_factories() {
	assert issue18077_validate_int(12345) == none
	assert issue18077_get_max_length_validator(10)('hello') != none
}
