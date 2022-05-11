fn func1() ?int {
	return 0
}

fn func2() ?(int, int) {
	return func1()?, 1
}

fn test_return_optional() ? {
	a, b := func2()?
	println('$a, $b')
	assert a == 0
	assert b == 1
}
