module math

fn test_jaccard() {
	assert jaccard_int([1, 2], [2, 3]) == 1.0 / 3.0
}
