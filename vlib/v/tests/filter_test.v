fn test_filter() {
	arr := [3, 6, 1, 8, 2, 4, 5, 7]
	filtered := arr.filter(it > 4)
	assert filtered.len == 4
	assert filtered.contains(6)
	assert filtered.contains(8)
	assert filtered.contains(5)
	assert filtered.contains(7)
}

fn test_shared_filter() {
	shared arr := [3, 6, 1, 8, 2, 4, 5, 7]

	filtered := rlock arr {
		arr.filter(it > 4)
	}
	assert filtered.len == 4
	assert filtered.contains(6)
	assert filtered.contains(8)
	assert filtered.contains(5)
	assert filtered.contains(7)
}
