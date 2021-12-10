// This file checks that string interpolations where expressions that generate
// multiple C statements work correctly
import json

fn test_array_map_interpolation() {
	numbers := [1, 2, 3]
	assert '${numbers.map(it * it)}' == '[1, 4, 9]'
}

fn test_json_encode_interpolation() {
	object := {
		'example': 'string'
		'other':   'data'
	}
	assert '${json.encode(object)}' == '{"example":"string","other":"data"}'
}
