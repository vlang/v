import json

struct TestOptionalRawString {
	id   int
	data ?string @[raw]
}

struct TestRawStringifiedObject {
	metadata string @[raw]
}

fn test_raw_opt() {
	test := TestOptionalRawString{
		id:   1
		data: 't
e
s
t'
	}
	encoded := json.encode(test)
	assert json.decode(TestOptionalRawString, encoded)!.data? == r'"t\ne\ns\nt"'
}

fn test_raw_none() {
	test := TestOptionalRawString{
		id:   1
		data: none
	}
	encoded := json.encode(test)
	r := json.decode(TestOptionalRawString, encoded)!.data
	assert r == none
}

fn test_raw_empty_string() {
	test := TestOptionalRawString{
		id:   1
		data: ''
	}
	encoded := json.encode(test)
	r := json.decode(TestOptionalRawString, encoded)!.data or { 'z' }
	assert r == '""'
}

fn test_stringified_object_returns_error_for_raw_field() {
	stringified_json :=
		json.encode('{"metadata":{"topLevelProperty":{"nestedProperty1":"Value 1"}}}')
	json.decode(TestRawStringifiedObject, stringified_json) or {
		assert err.msg().starts_with('Json element is not an object:')
		return
	}
	assert false
}
