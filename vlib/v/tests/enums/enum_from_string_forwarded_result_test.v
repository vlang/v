enum ForwardedChoice {
	zero
	one
	two = 7
}

enum ForwardedWideChoice as i64 {
	zero
	large = 123456789012345
}

fn parse_forwarded_choice(value string) !ForwardedChoice {
	return ForwardedChoice.from(value)
}

fn parse_forwarded_wide_choice(value string) !ForwardedWideChoice {
	return ForwardedWideChoice.from(value)
}

fn test_forwarded_enum_from_string_preserves_success_value() {
	assert parse_forwarded_choice('zero')! == .zero
	assert parse_forwarded_choice('one')! == .one
	assert parse_forwarded_choice('two')! == .two
	assert parse_forwarded_wide_choice('large')! == .large
}

fn test_forwarded_enum_from_string_preserves_error() {
	parse_forwarded_choice('invalid') or {
		assert err.msg() == 'invalid value'
		assert err.code() == 0
		return
	}
	assert false
}
