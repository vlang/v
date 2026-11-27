module math

fn test_elu() {
	assert elu(0, 1) == 0
	assert elu(2, 1) == 2
}
