module json2

import os

fn test_raw_decode_string() {
	str := raw_decode('"Hello!"') or {
		assert false
		Any(null)
	}
	assert str.str() == 'Hello!'
}

fn test_raw_decode_number() {
	num := raw_decode('123') or {
		assert false
		Any(null)
	}
	assert num.int() == 123
}

fn test_raw_decode_array() {
	raw_arr := raw_decode('["Foo", 1]') or {
		assert false
		Any(null)
	}
	arr := raw_arr.arr()
	assert arr[0].str() == 'Foo'
	assert arr[1].int() == 1
}

fn test_raw_decode_bool() {
	bol := raw_decode('false') or {
		assert false
		Any(null)
	}
	assert bol.bool() == false
}

fn test_raw_decode_map() {
	raw_mp := raw_decode('{"name":"Bob","age":20}') or {
		assert false
		Any(null)
	}
	mp := raw_mp.as_map()
	assert mp['name'].str() == 'Bob'
	assert mp['age'].int() == 20
}

fn test_raw_decode_null() {
	nul := raw_decode('null') or {
		assert false
		Any(null)
	}
	assert nul is Null
}

fn test_raw_decode_invalid() {
	raw_decode('1z') or {
		assert err.msg == '[x.json2] invalid token `z` (0:17)'
		return
	}
	assert false
}

fn test_raw_decode_string_with_dollarsign() {
	str := raw_decode(r'"Hello $world"') or {
		assert false
		Any(null)
	}
	assert str.str() == r'Hello $world'
}

fn test_raw_decode_map_with_whitespaces() {
	raw_mp := raw_decode(' \n\t{"name":"Bob","age":20}\n\t') or {
		eprintln(err.msg)
		assert false
		Any(null)
	}
	mp := raw_mp.as_map()
	assert mp['name'].str() == 'Bob'
	assert mp['age'].int() == 20
}

fn test_nested_array_map() ? {
	mut parser := new_parser('[[],[[]],[[[[]]]]]', false)
	decoded := parser.decode() ?
	assert parser.n_level == 0
}
