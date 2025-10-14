import x.json2 as json

enum Bar {
	a
	b
	c = 10
}

type BarAlias = Bar

fn test_number_decode() {
	assert json.decode[Bar]('0')! == Bar.a
	assert json.decode[Bar]('1')! == Bar.b
	assert json.decode[Bar]('10')! == Bar.c

	assert json.decode[BarAlias]('0')! == Bar.a
	assert json.decode[BarAlias]('1')! == Bar.b
	assert json.decode[BarAlias]('10')! == Bar.c
}

fn test_number_decode_fails() {
	if _ := json.decode[Bar]('2') {
		assert false
	} else {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Number value: `2` does not match any field in enum: &Bar'
		}
	}

	if _ := json.decode[BarAlias]('2') {
		assert false
	} else {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Number value: `2` does not match any field in enum: &BarAlias'
		}
	}
}

fn test_string_decode() {
	assert json.decode[Bar]('"a"')! == Bar.a
	assert json.decode[Bar]('"b"')! == Bar.b
	assert json.decode[Bar]('"c"')! == Bar.c

	assert json.decode[BarAlias]('"a"')! == Bar.a
	assert json.decode[BarAlias]('"b"')! == Bar.b
	assert json.decode[BarAlias]('"c"')! == Bar.c
}

fn test_string_decode_fails() {
	if _ := json.decode[Bar]('"d"') {
		assert false
	} else {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: String value: `d` does not match any field in enum: &Bar'
		}
	}

	if _ := json.decode[BarAlias]('"d"') {
		assert false
	} else {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: String value: `d` does not match any field in enum: &BarAlias'
		}
	}
}

fn test_invalid_decode_fails() {
	if _ := json.decode[Bar]('true') {
		assert false
	} else {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Expected number or string value for enum, got: boolean'
		}
	}

	if _ := json.decode[BarAlias]('true') {
		assert false
	} else {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Expected number or string value for enum, got: boolean'
		}
	}
}
