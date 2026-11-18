module math

fn test_softplus() {
	assert softplus(0) == math.log(2.0)
}

fn test_softplus_large_stays_finite() {
	assert softplus(1000) == 1000.0
	assert softplus(-1000) == 0.0
}
