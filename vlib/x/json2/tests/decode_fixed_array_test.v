import x.json2 as json

fn test_fixed_array() {
	mut expected := [3]int{}
	expected[0] = 1
	expected[1] = 2
	expected[2] = 3
	assert json.decode[[3]int]('[1, 2, 3]')! == expected
}

fn test_fixed_array_to_few() {
	json.decode[[4]int]('[1, 2, 3]') or {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 8
			assert err.message == 'Data: Fixed size array expected 4 elements but got 3 elements'
		}

		return
	}
	assert false
}

fn test_fixed_array_to_many() {
	json.decode[[2]int]('[1, 2, 3]') or {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 8
			assert err.message == 'Data: Fixed size array expected 2 elements but got 3 elements'
		}

		return
	}
	assert false
}
