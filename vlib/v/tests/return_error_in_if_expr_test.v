struct NotFoundError {
	Error
}

fn get_username() !string {
	return NotFoundError{}
}

fn print_username() !string {
	username := get_username() or {
		if err is NotFoundError {
			'test'
		} else {
			return err
		}
	}

	println(username)
	return username
}

fn test_return_err_in_if_expr() {
	ret := print_username()!
	assert ret == 'test'
}
