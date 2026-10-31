module math

fn test_dim() {
	assert dim(5, 3) == 2
	assert dim(3, 5) == 0
	assert dim(5, 5) == 0
	assert is_nan(dim(nan(), 1))
}
