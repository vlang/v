fn shift[T](mut a []T) T {
	res := a.first()
	a.drop(1)
	return res
}

fn test_generic_array_drop() {
	mut a := ['x', 'y']
	assert shift(mut a) == 'x' // 'x'
	assert a == ['y'] // ['y']
}
