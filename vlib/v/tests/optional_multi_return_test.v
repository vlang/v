fn tuple() ?(int, int) {
	return 1, 2
}

fn test_optional_multi_return() ? {
	println(tuple()?)
	a, b := tuple()?
	assert a == 1
	assert b == 2
}
