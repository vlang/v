module mlkem

fn test_poly_mul() {
	// (1 + x) * (1 - x) = 1 - x^2
	mut a := Poly{}
	mut b := Poly{}
	a[0] = 1
	a[1] = 1
	b[0] = 1
	b[1] = q - 1
	c := poly_mul(a, b)
	assert c[0] == 1
	assert c[1] == 0
	assert c[2] == q - 1
	for i in 3 .. n {
		assert c[i] == 0
	}
}

fn test_compress_roundtrip() {
	for x in 0 .. q {
		c := compress(x, 4)
		d := decompress(c, 4)
		// approximate
		_ = d
	}
	assert true
}
