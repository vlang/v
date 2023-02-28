struct CustomError {
	Error
}

fn ret() !string {
	return match true {
		true { 'ok' }
		else { CustomError{} }
	}
}

fn test_return_match_expr_with_custom_error() {
	if r := ret() {
		assert r == 'ok'
	} else {
		assert false
	}
}
