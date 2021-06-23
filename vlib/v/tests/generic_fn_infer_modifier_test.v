fn f_array<T>(a []T) T {
	return a[0]
}

fn g_array<T>(mut a []T) {
	a[0] = a[1]
}

fn test_array() {
	mut a := [7, 8]
	r := f_array(a)
	assert r == 7

	g_array(mut a)
	assert a[0] == 8
}
