fn get_opt(a int) ?string {
	if a < 0 {
		return none
	}
	return 'success'
}

fn get_opt_int(a int) ?int {
	if a < 0 {
		return none
	}
	return 12
}

fn foo(s ?string) int {
	if s as string != '' {
		return 1
	}
	return 0
}

fn test_option_unwrap_as_cast() {
	x := get_opt(1)
	d := get_opt_int(12)
	dump(d? as int == 12)
	dump('${x? as string}' == 'success')

	assert d? as int == 12
	assert x? as string == 'success'
}

fn test_option_unwrap_if_expr() {
	assert foo('') == 0
	assert foo('foo') == 1
	assert foo(none) == 0
}
