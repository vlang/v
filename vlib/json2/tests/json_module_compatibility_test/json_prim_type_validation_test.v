// vtest vflags: -w

module main

import json2

struct MyStruct {
	name   string // should fail
	age    ?int
	active bool
}

struct TestStructOne {
	property_one string @[json: 'propertyOne']
	property_two string @[json: 'propertyTwo']
}

fn test_main() {
	mut errors := 0
	json2.decode[MyStruct]('{ "name": 1}') or {
		errors++
		assert err.msg().contains('1:11: Invalid json: Data: Expected string, but got number')
	}
	json2.decode[MyStruct]('{ "name": "John Doe", "age": ""}') or {
		errors++
		assert err.msg().contains('empty string')
	}
	json2.decode[MyStruct]('{ "name": "John Doe", "age": 1, "active": ""}') or {
		errors++
		assert err.msg().contains('1:43: Invalid json: Data: Expected boolean, but got string')
	}
	res := json2.decode[MyStruct]('{ "name": "John Doe", "age": "1"}') or { panic(err) }
	assert errors == 3
	assert res.name == 'John Doe'
}

fn test_decode_object_into_string_field() {
	payload := '{"propertyOne":"property_two should stay a regular string {}","propertyTwo":{}}'
	res := json2.decode[TestStructOne](payload) or { panic(err) }
	assert res.property_one == 'property_two should stay a regular string {}'
	assert res.property_two == '{}'
}
