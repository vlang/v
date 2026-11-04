module stats

fn test_mean() {
	assert mean([1.0, 2.0, 3.0]) == 2.0
}

fn test_variance() {
	assert variance([1.0, 2.0, 3.0]) == 2.0 / 3.0
}

fn test_stddev() {
	assert stddev([1.0, 2.0, 3.0]) > 0
}
