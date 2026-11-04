module math

fn test_leaky_relu() {
	assert leaky_relu(-1, 0.01) == -0.01
	assert leaky_relu(2, 0.01) == 2
}
