struct Result {
	code int
}

fn result_payload() !Result {
	return Result{ code: 7 }
}

fn option_payload() ?Result {
	return Result{}
}

fn test_result_name_is_not_a_wrapper_tag() {
	plain := Result{}
	assert plain.code == 0
	assert result_payload()!.code == 7
	assert option_payload()?.code == 0
}

struct Optional {
	code int
}

fn named_optional_payload() ?Optional {
	return Optional{ code: 11 }
}

fn named_optional_result() !Optional {
	return Optional{ code: 13 }
}

fn test_optional_name_is_not_a_wrapper_tag() {
	plain := Optional{}
	assert plain.code == 0
	assert named_optional_payload()?.code == 11
	assert named_optional_result()!.code == 13
}
