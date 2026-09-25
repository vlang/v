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

fn checked_title_expr(req IfGuardAfterNoneCheck) !string {
	if req.title == none {
		return error('title is required')
	}
	title := if value := req.title { value.trim_space() } else { '' }
	return title
}

fn test_if_expr_guard_after_none_check_uses_optional_wrapper() {
	assert checked_title_expr(IfGuardAfterNoneCheck{ title: '  hello  ' })! == 'hello'
	assert (checked_title_expr(IfGuardAfterNoneCheck{}) or { err.msg() }) == 'title is required'
}

fn checked_title_return_expr(req IfGuardAfterNoneCheck) !string {
	if req.title == none {
		return error('title is required')
	}
	return if value := req.title { value.trim_space() } else { '' }
}

fn test_return_if_expr_guard_after_none_check_uses_optional_wrapper() {
	assert checked_title_return_expr(IfGuardAfterNoneCheck{ title: '  hello  ' })! == 'hello'
	assert (checked_title_return_expr(IfGuardAfterNoneCheck{}) or { err.msg() }) == 'title is required'
}

fn checked_title_else_if_return_expr(req IfGuardAfterNoneCheck, flag bool) !string {
	if req.title == none {
		return error('title is required')
	}
	return if flag {
		'fixed'
	} else if value := req.title {
		value.trim_space()
	} else {
		''
	}
}

fn test_return_else_if_guard_after_none_check_uses_optional_wrapper() {
	req := IfGuardAfterNoneCheck{ title: '  hello  ' }
	assert checked_title_else_if_return_expr(req, false)! == 'hello'
	assert checked_title_else_if_return_expr(req, true)! == 'fixed'
	assert (checked_title_else_if_return_expr(IfGuardAfterNoneCheck{}, false) or { err.msg() }) == 'title is required'
}

fn checked_parenthesized_title(req IfGuardAfterNoneCheck) !string {
	if req.title == none {
		return error('title is required')
	}
	if value := (req.title) {
		return value.trim_space()
	}
	return error('title is required')
}

fn checked_parenthesized_title_expr(req IfGuardAfterNoneCheck) !string {
	if req.title == none {
		return error('title is required')
	}
	return if value := (req.title) { value.trim_space() } else { '' }
}

fn test_parenthesized_if_guards_after_none_check_use_optional_wrapper() {
	req := IfGuardAfterNoneCheck{ title: '  hello  ' }
	assert checked_parenthesized_title(req)! == 'hello'
	assert checked_parenthesized_title_expr(req)! == 'hello'
	assert (checked_parenthesized_title(IfGuardAfterNoneCheck{}) or { err.msg() }) == 'title is required'
}

struct PromotedOptionalFieldInner {
mut:
	value ?string
}

struct PromotedOptionalFieldOuter {
	PromotedOptionalFieldInner
}

fn promoted_field_guard(outer PromotedOptionalFieldOuter) string {
	if value := outer.value {
		return value
	}
	return ''
}

fn promoted_field_guard_expr(outer PromotedOptionalFieldOuter) string {
	return if value := outer.value { value } else { '' }
}

fn promoted_field_guard_after_none(outer PromotedOptionalFieldOuter) !string {
	if outer.value == none {
		return error('value is required')
	}
	if value := outer.value {
		return value
	}
	return error('value is required')
}

fn promoted_field_guard_expr_after_none(outer PromotedOptionalFieldOuter) !string {
	if outer.value == none {
		return error('value is required')
	}
	return if value := outer.value { value } else { '' }
}

fn test_if_guards_lower_promoted_optional_field() {
	mut outer := PromotedOptionalFieldOuter{}
	outer.PromotedOptionalFieldInner.value = 'hello'
	assert promoted_field_guard(outer) == 'hello'
	assert promoted_field_guard_expr(outer) == 'hello'
	assert promoted_field_guard_after_none(outer)! == 'hello'
	assert promoted_field_guard_expr_after_none(outer)! == 'hello'
	if value := outer.value {
		assert value == 'hello'
	} else {
		assert false
	}
	result := if value := outer.value { value } else { '' }
	assert result == 'hello'
	empty := PromotedOptionalFieldOuter{}
	assert promoted_field_guard(empty) == ''
	assert promoted_field_guard_expr(empty) == ''
	assert (promoted_field_guard_after_none(empty) or { err.msg() }) == 'value is required'
	assert (promoted_field_guard_expr_after_none(empty) or { err.msg() }) == 'value is required'
}

struct SharedOptionalField {
mut:
	value ?string
}

struct SharedOptionalFieldA {
	SharedOptionalField
}

struct SharedOptionalFieldB {
	SharedOptionalField
}

type SharedOptionalFieldParam = SharedOptionalFieldA | SharedOptionalFieldB

fn shared_field_guard_after_none(param SharedOptionalFieldParam) !string {
	if param.value == none {
		return error('value is required')
	}
	if value := param.value {
		return value
	}
	return error('value is required')
}

fn shared_field_guard_expr_after_none(param SharedOptionalFieldParam) !string {
	if param.value == none {
		return error('value is required')
	}
	return if value := param.value { value } else { '' }
}

fn test_if_guards_lower_shared_optional_sum_field() {
	mut a := SharedOptionalFieldA{}
	a.SharedOptionalField.value = 'hello'
	param := SharedOptionalFieldParam(a)
	assert shared_field_guard_after_none(param)! == 'hello'
	assert shared_field_guard_expr_after_none(param)! == 'hello'
	empty := SharedOptionalFieldParam(SharedOptionalFieldB{})
	assert (shared_field_guard_after_none(empty) or { err.msg() }) == 'value is required'
	assert (shared_field_guard_expr_after_none(empty) or { err.msg() }) == 'value is required'
}
