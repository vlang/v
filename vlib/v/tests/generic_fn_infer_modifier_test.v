fn f_array[T](a []T) T {
	return a[0]
}

fn g_array[T](mut a []T) {
	a[0] = a[1]
}

fn test_array() {
	// []int
	mut a1 := [7, 8]
	r1 := f_array(a1)
	assert r1 == 7

	g_array(mut a1)
	assert a1[0] == 8

	// []f64
	mut a2 := [1.1, 2.2]
	r2 := f_array(a2)
	assert r2 == 1.1

	g_array(mut a2)
	assert a2[0] == 2.2

	// []string
	mut a3 := ['aa', 'bb']
	r3 := f_array(a3)
	assert r3 == 'aa'

	g_array(mut a3)
	assert a3[0] == 'bb'

	// []bool
	mut a4 := [true, false]
	r4 := f_array(a4)
	assert r4 == true

	g_array(mut a4)
	assert a4[0] == false
}
