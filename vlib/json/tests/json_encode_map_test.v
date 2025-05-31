import json

struct Test {
	optional_string       ?string
	optional_array        ?[]string
	optional_struct_array ?[]string
	optional_map          ?map[string]string
}

fn test_main() {
	test := Test{}
	encoded := json.encode(test)
	assert dump(encoded) == '{}'

	test2 := Test{
		optional_map: {
			'foo': 'bar'
		}
	}
	encoded2 := json.encode(test2)
	assert dump(encoded2) == '{"optional_map":{"foo":"bar"}}'
}
