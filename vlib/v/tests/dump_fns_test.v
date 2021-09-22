fn foo() int {
	return 1
}

fn zoo() int {
	return 123
}

fn test_dump_of_functions() {
	x := dump(foo)
	y := dump(zoo)
	dump(foo())
	dump(zoo())
	dump(x)
	dump(y)
	dump(x())
	dump(y())
	assert voidptr(x) != 0
	assert voidptr(y) != 0
	assert foo == x
	assert y == zoo
}
