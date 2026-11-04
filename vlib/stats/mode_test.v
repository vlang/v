module stats

fn test_mode() {
	assert mode([1.0, 2.0, 2.0, 3.0]) == 2.0
}
