fn test_anon_fn_option_call_in_if_expr_1() {
	if fn () ?bool {
		return true
	}() or { false }
	{
		println('ok')
		assert true
	} else {
		assert false
	}
}

fn test_anon_fn_option_call_in_if_expr_2() {
	if fn () ?bool {
		return true
	}()?
	{
		println('ok')
		assert true
	} else {
		assert false
	}
}

fn test_anon_fn_option_call_in_if_expr_3() {
	if fn () !bool {
		return true
	}()!
	{
		println('ok')
		assert true
	} else {
		assert false
	}
}
