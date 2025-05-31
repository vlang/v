fn multret1(i int, j int) (int, int) {
	return if i > j { i, 10 } else { 10, j }
}

fn multret2(i int, j int) (int, int) {
	return match i > j {
		true { i, 10 }
		false { 10, j }
	}
}

fn multret3(i int, j int) (int, int) {
	if i > j {
		return i, 10
	} else {
		return 10, j
	}
}

fn multret4(i int, j int) (int, int) {
	match i > j {
		true { return i, 10 }
		false { return 10, j }
	}
}

fn test_fn_multi_return() {
	mut a, mut b := 0, 0

	println(multret1(3, 14))
	a, b = multret1(3, 14)
	assert a == 10
	assert b == 14

	println(multret2(3, 14))
	a, b = multret2(3, 14)
	assert a == 10
	assert b == 14

	println(multret3(3, 14))
	a, b = multret3(3, 14)
	assert a == 10
	assert b == 14

	println(multret4(3, 14))
	a, b = multret4(3, 14)
	assert a == 10
	assert b == 14
}
