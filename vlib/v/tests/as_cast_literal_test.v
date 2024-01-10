interface IExample {}

fn thing(a IExample) bool {
	return true
}

fn test_as_cast_with_literals() {
	assert thing(true)
	assert thing(123)
	assert thing(12.3)
	assert thing('abc')
}
