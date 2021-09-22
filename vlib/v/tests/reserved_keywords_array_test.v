fn test_reserved_keywords_array_and_string() {
	array := [1, 2, 3, 4]
	mut res1 := array.map(it * 3)
	mut res2 := array.filter(it > 2)
	println(res1)
	assert res1 == [3, 6, 9, 12]
	println(res2)
	assert res2 == [3, 4]
}
