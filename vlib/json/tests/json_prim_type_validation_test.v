module main

import json

struct MyStruct {
	name   string // should fail
	age    ?int
	active bool
}

fn test_main() {
	mut errors := 0
	json.decode(MyStruct, '{ "name": 1}') or {
		errors++
		assert err.msg() == "type mismatch for field 'name', expecting `string` type, got: 1"
	}
	json.decode(MyStruct, '{ "name": "John Doe", "age": ""}') or {
		errors++
		assert err.msg() == 'type mismatch for field \'age\', expecting `?int` type, got: ""'
	}
	json.decode(MyStruct, '{ "name": "John Doe", "age": 1, "active": ""}') or {
		errors++
		assert err.msg() == 'type mismatch for field \'active\', expecting `bool` type, got: ""'
	}
	res := json.decode(MyStruct, '{ "name": "John Doe", "age": "1"}') or { panic(err) }
	assert errors == 3
}
