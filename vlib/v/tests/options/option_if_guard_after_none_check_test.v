struct IfGuardAfterNoneCheck {
	title ?string
}

fn checked_title(req IfGuardAfterNoneCheck) !string {
	if req.title == none {
		return error('title is required')
	}
	if title := req.title {
		return title.trim_space()
	}
	return error('title is required')
}

fn test_if_guard_after_none_check_uses_optional_wrapper() {
	assert checked_title(IfGuardAfterNoneCheck{ title: '  hello  ' })! == 'hello'
	assert (checked_title(IfGuardAfterNoneCheck{}) or { err.msg() }) == 'title is required'
}
