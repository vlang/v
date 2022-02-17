fn test_shared_map() {
	shared a := ['bbbb', 'cc', 'ddd']
	rlock a {
		assert a.map(it.len) == [4, 2, 3]
	}
}
