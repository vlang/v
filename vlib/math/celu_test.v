module math

fn test_celu() {
	assert celu(0, 1) == 0
	assert celu(2, 1) == 2
}
