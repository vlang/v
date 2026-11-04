module strconv

fn test_format_bool() {
	assert format_bool(true) == 'true'
	assert format_bool(false) == 'false'
}
