module math

fn test_extended_gcd() {
	g, x, y := extended_gcd(30, 12)
	assert g == 6
	assert 30 * x + 12 * y == g
}
