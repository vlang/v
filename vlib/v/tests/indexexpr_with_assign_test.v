fn test_main() {
	mut test := []f64{}
	test << 0
	test[0] = if true {
		sum := 0.0
		sum
	} else {
		0
	}
	assert test == [0.0]
}
