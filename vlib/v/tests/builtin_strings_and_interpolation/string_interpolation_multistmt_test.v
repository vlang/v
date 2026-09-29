// vtest vflags: -w

// This file checks that string interpolations where expressions that generate
// multiple C statements work correctly
import json2

fn test_array_map_interpolation() {
	numbers := [1, 2, 3]
	assert '${numbers.map(it * it)}' == '[1, 4, 9]'
}

fn test_json_encode_interpolation() {
	object := {
		'example': 'string'
		'other':   'data'
	}
	assert '${json2.encode(object, escape_unicode: true)}' == '{"example":"string","other":"data"}'
}
