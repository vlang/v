fn show(a string) string {
	return a
}

fn test_function_interpolation() {
	f := fn()(string, bool) {return 'aaa', true}
	println(f)
	assert '$f' == 'fn () (string, bool)'

	println(show)
	assert '$show' == 'fn (string) string'
}
