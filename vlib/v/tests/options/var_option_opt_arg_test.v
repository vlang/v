fn option_arg(x ?int) ?int {
	assert x != none
	return x
}

fn option_arg2(x ?f64, y ?int, z ?string) ?string {
	assert x != none
	assert y != none
	assert z != none
	return z
}

fn option_arg3(x ?f64, y ?int, z ?string) bool {
	assert x == none
	assert y == none
	assert z == none
	return true
}

fn test_main() {
	var := ?int(1)
	assert option_arg(var)? == 1
	assert option_arg(100)? == 100
	assert option_arg2(1.1, 1, '')? == ''
	assert option_arg2(1.2, 2, 'foo')? == 'foo'
	assert option_arg3(none, none, none)
}
