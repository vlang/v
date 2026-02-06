fn ret_opt() ?string {
	return 'foo'
}

fn option_arg(x ?int) (?string, ?int) {
	assert x == none
	return '1', x
}

fn test_main() {
	var := ret_opt()?
	assert var == 'foo'

	var1, var2 := option_arg(none)
	assert var1 != none
	assert var2 == none
}
