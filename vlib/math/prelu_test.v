module math

fn test_prelu() {
	assert prelu(-1, 0.25) == -0.25
	assert prelu(2, 0.25) == 2
}
