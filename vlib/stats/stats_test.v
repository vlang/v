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

fn test_median() {
	assert median([1.0, 2.0, 3.0]) == 2.0
	assert median([1.0, 2.0, 3.0, 4.0]) == 2.5
}

fn test_quantile() {
	assert quantile([1.0, 2.0, 3.0, 4.0], 0.5) == 2.5
}
