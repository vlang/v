fn optional_arg(x ?int) {
	println('int type')
	assert true
}

fn test_fn_arg_of_optional() {
	optional_arg(1)
	optional_arg(none)
}
