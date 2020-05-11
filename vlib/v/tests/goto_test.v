fn test_goto() {
	mut x := 0
	a: b := 1
	_ = b
	x++
	if x < 3 {
		goto a
	}
	assert x == 3
}
