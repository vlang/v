fn foo(fail bool) ?string {
	return if fail { error('failure') } else { 'success' }
}

fn test_assert_fn_call_with_parentheses() {
	assert (foo(true) or { '' }) == ''
}
