fn g<T>(arr []T) {
	mut r := []T{}
	r << arr
	assert arr.len > 0
}

fn test_generic_array_append() {
	g([1, 2, 3])
	g([1.1, 2.2, 3.3])
	g(['aa', 'bb', 'cc'])
}
