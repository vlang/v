fn test_and_and_not_parses() {
	ok1 := true
	ok2 := false && !ok1
	assert ok2 == false
}
