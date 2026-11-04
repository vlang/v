module math

fn test_bce() {
	assert binary_cross_entropy(1, 1) < 1e-9
}
