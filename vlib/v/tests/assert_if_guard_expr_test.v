fn foo() !int {
	return error('error')
}

fn test_assert_if_guard_expr() {
	assert if _ := foo() {
		false
	} else {
		true
	}
}
