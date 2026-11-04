module arrays

fn test_last_index_of() {
	assert last_index_of([1, 2, 1], 1) == 2
	assert last_index_of([1, 2, 3], 4) == -1
}
