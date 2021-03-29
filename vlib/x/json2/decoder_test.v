import x.json2

fn test_raw_decode_string() {
	str := json2.raw_decode('"Hello!"') or {
		assert false
		json2.Any{}
	}
	assert str.str() == 'Hello!'
}

fn test_raw_decode_number() {
	num := json2.raw_decode('123') or {
		assert false
		json2.Any{}
	}
	assert num.int() == 123
}

fn test_raw_decode_array() {
	raw_arr := json2.raw_decode('["Foo", 1]') or {
		assert false
		json2.Any{}
	}
	arr := raw_arr.arr()
	assert arr[0].str() == 'Foo'
	assert arr[1].int() == 1
}

fn test_raw_decode_bool() {
	bol := json2.raw_decode('false') or {
		assert false
		json2.Any{}
	}
	assert bol.bool() == false
}

fn test_raw_decode_map() {
	raw_mp := json2.raw_decode('{"name":"Bob","age":20}') or {
		assert false
		json2.Any{}
	}
	mp := raw_mp.as_map()
	assert mp['name'].str() == 'Bob'
	assert mp['age'].int() == 20
}

fn test_raw_decode_null() {
	nul := json2.raw_decode('null') or {
		assert false
		json2.Any{}
	}
	assert nul is json2.Null
}

fn test_raw_decode_invalid() {
	json2.raw_decode('1z') or {
		assert err.msg == '[x.json2] invalid token `z` (0:17)'
		return
	}
	assert false
}

fn test_raw_decode_string_with_dollarsign() {
	str := json2.raw_decode(r'"Hello $world"') or {
		assert false
		json2.Any{}
	}
	assert str.str() == r'Hello $world'
}

fn test_raw_decode_map_with_whitespaces() {
	raw_mp := json2.raw_decode(' \n\t{"name":"Bob","age":20}\n\t') or {
		eprintln(err.msg)
		assert false
		json2.Any{}
	}
	mp := raw_mp.as_map()
	assert mp['name'].str() == 'Bob'
	assert mp['age'].int() == 20
}
