module math

fn test_trapz() {
	// integral of x from 0 to 1 approximated
	y := [0.0, 0.5, 1.0]
	assert trapz(y, 0.5) == 0.5
}
