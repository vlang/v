// verify fix for #2913
fn some_multiret_fn(a int, b int) (int, int) {
	return a + 1, b + 1
}

fn test_repeated_multiple_multiret() {
	a, b := some_multiret_fn(1, 2)
	assert a == 2
	assert b == 3
	c, d := some_multiret_fn(3, 4)
	assert c == 4
	assert d == 5
}
