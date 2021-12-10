type Tuple = []f64

fn new_tuple(x f64, y f64, z f64, w f64) Tuple {
	return Tuple([x, y, z, w])
}

fn (a Tuple) + (b Tuple) Tuple {
	mut res := []f64{len: a.len}
	for i := 0; i < a.len; i++ {
		res[i] = a[i] + b[i]
	}
	return Tuple(res)
}

fn test_tuple_arithmetic() {
	a := new_tuple(3, -2, 5, 1)
	b := new_tuple(-2, 3, 1, 0)
	assert a + b == new_tuple(1, 1, 6, 1)
}
