fn foo_res() ![]string {
	return []
}

fn foo_opt() ?[]string {
	return []
}

fn test_fn_return_opt_or_res_of_array() {
	foo_res() or { panic(err) }
	foo_opt() or { panic(err) }
	assert true
}
