struct CustomError {
	Error
}

fn inner_custom_error() !int {
	return error('boom')
}

fn outer_custom_error() !int {
	return inner_custom_error() or { CustomError{} }
}

fn test_return_or_block_with_custom_error() {
	if _ := outer_custom_error() {
		assert false
	} else {
		assert err is CustomError
	}
}
