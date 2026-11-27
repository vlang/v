module math

fn test_polyval() {
	// 2x^2 + 3x + 1 at x=2 => 8+6+1=15
	assert polyval([2.0, 3.0, 1.0], 2.0) == 15.0
}
