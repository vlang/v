fn test_1() {
	mut test := []f64{}
	test << 0
	test[0] = if true {
		sum := 1.2
		sum
	} else {
		0
	}
	assert test == [1.2]
}

fn test_2() {
	check := true
	mut test := []f64{len: 100}
	for i in 0 .. 100 {
		test[i] = if check {
			mut sum := 0.0
			for x in 0 .. 100 {
				if check {
					sum += 1
				}
				if i <= x {
					break
				}
			}
			sum
		} else {
			0
		}
	}
	assert test[0] == 1.0
	assert test[99] == 100.0
}
