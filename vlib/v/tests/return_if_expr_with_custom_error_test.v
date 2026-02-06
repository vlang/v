struct CustomError {
	Error
}

fn ret() !string {
	return if true {
		'ok'
	} else {
		CustomError{}
	}
}

fn test_return_if_expr_with_custom_error() {
	if r := ret() {
		assert r == 'ok'
	} else {
		assert false
	}
}
