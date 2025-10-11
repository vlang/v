fn test_main() {
	a := [1, 2, 3]!
	assert a.filter(it > 2).len == 1
}
