module math

fn test_cumsum() {
	assert cumsum([1.0, 2.0, 3.0]) == [1.0, 3.0, 6.0]
	assert cumprod([1.0, 2.0, 3.0]) == [1.0, 2.0, 6.0]
}
