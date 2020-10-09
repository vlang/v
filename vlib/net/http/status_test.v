module http

fn test_str() {
	code := Status.bad_gateway
	actual := code.str()
	assert actual == 'Bad Gateway'
}

fn test_int() {
	code := Status.see_other
	actual := code.int()
	assert actual == 303
}

fn test_is_valid_positive() {
	code := Status.unassigned
	actual := code.is_valid()
	assert !actual
}

fn test_is_valid_negative() {
	code := Status.gateway_timeout
	actual := code.is_valid()
	assert actual
}

fn test_is_error_positive() {
	code := Status.too_many_requests
	actual := code.is_valid()
	assert !actual
}

fn test_is_error_negative() {
	code := Status.cont
	actual := code.is_valid()
	assert actual
}

fn test_is_success_positive() {
	code := Status.accepted
	actual := code.is_valid()
	assert actual
}

fn test_is_success_negative() {
	code := Status.forbidden
	actual := code.is_valid()
	assert !actual
}
