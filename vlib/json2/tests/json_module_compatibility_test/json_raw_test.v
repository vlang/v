// vtest vflags: -w
import json2

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
	encoded := json2.encode(test, escape_unicode: true)
	assert json2.decode[TestOptionalRawString](encoded)!.data? == r'"t\ne\ns\nt"'
}

fn test_raw_none() {
	test := TestOptionalRawString{
		id:   1
		data: none
	}
	encoded := json2.encode(test, escape_unicode: true)
	r := json2.decode[TestOptionalRawString](encoded)!.data
	assert r == none
}

fn test_raw_empty_string() {
	test := TestOptionalRawString{
		id:   1
		data: ''
	}
	encoded := json2.encode(test, escape_unicode: true)
	r := json2.decode[TestOptionalRawString](encoded)!.data or { 'z' }
	assert r == '""'
}

fn test_stringified_object_returns_error_for_raw_field() {
	stringified_json :=
		json2.encode('{"metadata":{"topLevelProperty":{"nestedProperty1":"Value 1"}}}',
			escape_unicode: true
		)
	json2.decode[TestRawStringifiedObject](stringified_json) or {
		assert err.msg().contains('Invalid json: Data: Expected object, but got string')
		return
	}
	assert false
}
