// verify fix for #2913
fn some_multiret_fn(a, b int) (int, int) {
	return a + 1, b + 1
}

fn test_reuse_multiple_multiret() {
	mut c, mut d := some_multiret_fn(4, 10)
	mut a, mut b := some_multiret_fn(c, d)
	assert a == c + 1
	assert b == d + 1
	for i in 1 .. 10 {
		c += i
		d += i
		a, b = some_multiret_fn(c, d)
		assert a == c + 1
		assert b == d + 1
		c += i + 1
		d += i + 1
		a, b = some_multiret_fn(c, d)
		assert a == c + 1
		assert b == d + 1
	}
}
