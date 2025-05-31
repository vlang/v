fn foo() !int {
	return 0
}

fn bar(n int) bool {
	return true
}

fn is_ok(b bool) !bool {
	return if b {
		bar(foo()!)
	} else {
		true
	}
}

fn test_if_expr_with_fn_call_result() {
	ret := is_ok(true) or { false }
	println(ret)
	assert ret
}
