import x.json2 as json

fn test_raw_decode_string() {
	str := json.decode[json.Any]('"Hello!"')!
	assert str.str() == 'Hello!'
}

fn test_raw_decode_string_escape() {
	jstr := json.decode[json.Any]('"\\u001b"')!
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

fn test_raw_decode_invalid() {
	json.decode[json.Any]('1z') or {
		assert err.msg().contains('1:2: Invalid json: Syntax: invalid value. Unexpected character after number end')
		return
	}
	assert false
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
		assert err.msg().contains('1:8: Invalid json: Syntax: expected `:`, got `,`')
		return
	}
	assert false
}

fn test_raw_decode_array_invalid() {
	json.decode[json.Any]('["Foo", 1,}') or {
		assert err.msg().contains('1:11: Invalid json: Syntax: unknown value kind')
		return
	}
	assert false
}

struct Foo {
	int  []int
	str  []string
	f32  []f32
	oint []?int
}

// fn test_decode_array_fields() {
//	input := '{"int":[0, 1], "str":["2", "3"], "f32": [4.0, 5.0], "oint": [6, null]}'
//	foo := json.decode[Foo](input)!
//	assert foo.int == [0, 1]
//	assert foo.str == ['2', '3']
//	assert foo.f32 == [f32(4.0), 5.0]
//	a, b := foo.oint[0], foo.oint[1]
//	assert a? == 6
//	assert b? == 0
//}

struct ContactItem {
	description string
	telnr       string
}

struct User {
	name    string
	age     int
	contact ContactItem
}

fn test_decode_missing_comma() {
	data := '{
				"name": "Frodo",
				"age": 25
				"contact": {
					"description": "descr",
					"telnr": "+32333"
				}
			}'
	user := json.decode[User](data) or { return }
}
