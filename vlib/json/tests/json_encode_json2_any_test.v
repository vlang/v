import json
import x.json2

fn test_encode_map_with_json2_any_values() {
	data := {
		'array':  json2.Any([json2.Any('x'), json2.Any(false)])
		'bool':   json2.Any(true)
		'number': json2.Any(f64(1.5))
		'object': json2.Any({
			'nested': json2.Any('ok')
		})
		'string': json2.Any('hello')
	}

	encoded := json.encode(data)
	decoded := json2.decode[map[string]json2.Any](encoded)!
	array := decoded['array'] or { panic('missing array') }
	bool_val := decoded['bool'] or { panic('missing bool') }
	number := decoded['number'] or { panic('missing number') }
	object := decoded['object'] or { panic('missing object') }
	nested := object.as_map()['nested'] or { panic('missing nested') }
	string_val := decoded['string'] or { panic('missing string') }
	assert array.as_array()[0].str() == 'x'
	assert array.as_array()[1].bool() == false
	assert bool_val.bool() == true
	assert number.f64() == 1.5
	assert nested.str() == 'ok'
	assert string_val.str() == 'hello'
}
