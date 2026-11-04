module math

fn test_prod() {
	assert prod([2.0, 3.0, 4.0]) == 24.0
	assert prod([]f64{}) == 1.0
}
