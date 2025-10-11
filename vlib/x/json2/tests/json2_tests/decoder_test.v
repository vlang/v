import x.json2 as json

fn test_raw_decode_string() {
	str := json.decode[json.Any]('"Hello!"')!
	assert str.str() == 'Hello!'
}

fn test_raw_decode_string_escape() {
	jstr := json.decode[json.Any]('"\u001b"')!
	str := jstr.str()
	assert str.len == 1
	assert str[0] == 27
}

fn test_raw_decode_number() {
	num := json.decode[json.Any]('123')!
	assert num.int() == 123
}

fn test_raw_decode_array() {
	raw_arr := json.decode[json.Any]('["Foo", 1]')!
	arr := raw_arr.arr()
	assert arr[0] or { 0 }.str() == 'Foo'
	assert arr[1] or { 0 }.int() == 1
}

fn test_raw_decode_bool() {
	bol := json.decode[json.Any]('false')!
	assert bol.bool() == false
}

fn test_raw_decode_map() {
	raw_mp := json.decode[json.Any]('{"name":"Bob","age":20}')!
	mp := raw_mp.as_map()
	assert mp['name'] or { 0 }.str() == 'Bob'
	assert mp['age'] or { 0 }.int() == 20
}

fn test_raw_decode_string_with_dollarsign() {
	str := json.decode[json.Any](r'"Hello $world"')!
	assert str.str() == r'Hello $world'
}

fn test_raw_decode_map_with_whitespaces() {
	raw_mp := json.decode[json.Any](' \n\t{"name":"Bob","age":20}\n\t')!
	mp := raw_mp.as_map()
	assert mp['name'] or { 0 }.str() == 'Bob'
	assert mp['age'] or { 0 }.int() == 20
}

fn test_raw_decode_map_invalid() {
	json.decode[json.Any]('{"name","Bob","age":20}') or {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 8
			assert err.message == 'Syntax: expected `:`, got `,`'
		}

		return
	}
	assert false
}

fn test_raw_decode_array_invalid() {
	json.decode[json.Any]('["Foo", 1,}') or {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 11
			assert err.message == 'Syntax: unknown value kind'
		}

		return
	}
	assert false
}
