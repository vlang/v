struct LocalDecodePayload {
	value string
}

fn json__decode(source string) !LocalDecodePayload {
	if source.len == 0 {
		return error('empty input')
	}
	return LocalDecodePayload{
		value: source
	}
}

fn test_double_underscore_decode_name_is_an_ordinary_function() {
	payload := json__decode('ordinary call')!
	assert payload.value == 'ordinary call'
}
