fn test_option_if_expr_with_panic_call() {
	mut x := ?string(none)
	x = if true {
		''
	} else {
		panic('')
	}
	println(x)
	assert true
}
