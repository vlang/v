struct Options {
	option_1 ?[]int
	option_2 ?[]int
}

fn test_none() {
	values := Options{
		option_1: [1, 2, 3]
		option_2: none
	}
	mut res := 0.0
	if values.option_1 != none && values.option_2 != none {
		res = values.option_1[0] + values.option_2[0]
	}
	assert res == 0
}

fn test_add() {
	values := Options{
		option_1: [1, 2, 3]
		option_2: [3]
	}
	mut res := 0.0
	if values.option_1 != none && values.option_2 != none {
		res = values.option_1[0] + values.option_2[0]
	}
	assert res == 4
}
