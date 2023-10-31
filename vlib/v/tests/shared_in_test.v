fn test_shared_in() {
	shared a := [1, 3, 7, 3]
	rlock a {
		assert 1 in a
		assert 0 !in a
		assert 7 in a
		assert 3 in a
		assert 1238941 !in a
	}
}
