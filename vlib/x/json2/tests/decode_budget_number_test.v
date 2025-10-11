import x.json2 as json

fn test_budget_number() {
	assert json.decode[int]('"0"')! == 0
	assert json.decode[int]('"100"')! == 100
	assert json.decode[f64]('"-23.6e1"')! == -236.0

	assert json.decode[[]int]('["100", 99, "98", 97]')! == [100, 99, 98, 97]
}

fn test_budget_number_malformed() {
	json.decode[int]('"+100"') or {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 2
			assert err.message == 'Syntax: expected digit got +'
		}

		return
	}

	assert false
}
