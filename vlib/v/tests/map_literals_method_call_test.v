fn test_map_literals_method_call() {
	a := 1 in map{
		1: 1
	}.keys().map(map{
		1: 1
	}[it])

	println(a)
	assert a
}
