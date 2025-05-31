fn test_map_literals_method_call() {
	a := 1 in {
		1: 1
	}.keys().map({
		1: 1
	}[it])

	println(a)
	assert a
}
