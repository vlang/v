fn test_map_fn() {
	map1 := [3, 4, 6]
	assert map1.map(it * 2) == [6, 8, 12]
	shared map2 := [7, 9, 10]
	rlock map2 {
		assert map2.map(it * 2) == [14, 18, 20]
	}
}
