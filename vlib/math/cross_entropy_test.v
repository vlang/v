module math

fn test_ce() {
	assert cross_entropy([1.0, 0.0], [1.0, 0.0]) < 1e-9
}
