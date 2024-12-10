import x.json2.decoder2 as json
import x.json2

fn test_raw_decode_string() {
	str := json.decode[json2.Any]('"Hello!"')!
	assert str.str() == 'Hello!'
}

fn test_raw_decode_string_escape() {
	jstr := json.decode[json2.Any]('"\u001b"')!
	str := jstr.str()
	assert str.len == 1
	assert str[0] == 27
}

fn test_raw_decode_number() {
	num := json.decode[json2.Any]('123')!
	assert num.int() == 123
}

fn test_raw_decode_array() {
	raw_arr := json.decode[json2.Any]('["Foo", 1]')!
	arr := raw_arr.arr()
	assert arr[0] or { 0 }.str() == 'Foo'
	assert arr[1] or { 0 }.int() == 1
}

fn test_raw_decode_bool() {
	bol := json.decode[json2.Any]('false')!
	assert bol.bool() == false
}

fn test_raw_decode_map() {
	raw_mp := json.decode[json2.Any]('{"name":"Bob","age":20}')!
	mp := raw_mp.as_map()
	assert mp['name'] or { 0 }.str() == 'Bob'
	assert mp['age'] or { 0 }.int() == 20
}

fn test_raw_decode_string_with_dollarsign() {
	str := json.decode[json2.Any](r'"Hello $world"')!
	assert str.str() == r'Hello $world'
}

fn test_raw_decode_map_with_whitespaces() {
	raw_mp := json.decode[json2.Any](' \n\t{"name":"Bob","age":20}\n\t')!
	mp := raw_mp.as_map()
	assert mp['name'] or { 0 }.str() == 'Bob'
	assert mp['age'] or { 0 }.int() == 20
}

fn test_nested_array_object() {
	mut parser := json2.new_parser(r'[[[[[],[],[]]]],{"Test":{}},[[]]]', false)
	decoded := parser.decode()!
	assert parser.n_level == 0
}

fn test_raw_decode_map_invalid() {
	json.decode[json2.Any]('{"name","Bob","age":20}') or {
		assert err.msg() == '\n{"name",\n       ^ invalid value after object key'

		return
	}
	assert false
}

fn test_raw_decode_array_invalid() {
	json.decode[json2.Any]('["Foo", 1,}') or {
		assert err.msg() == '\n["Foo", 1,}\n          ^ EOF error: array not closed'

		return
	}
	assert false
}
