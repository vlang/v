module math

fn test_crt() {
	x, lcm, ok := crt(2, 3, 3, 5)
	assert ok
	assert x == 8
	assert lcm == 15
}
