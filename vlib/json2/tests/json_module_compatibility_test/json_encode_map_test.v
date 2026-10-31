// vtest vflags: -w
import json2

struct Test {
	optional_string       ?string
	optional_array        ?[]string
	optional_struct_array ?[]string
	optional_map          ?map[string]string
}

fn test_main() {
	test := Test{}
	encoded := json2.encode(test, escape_unicode: true)
	assert dump(encoded) == '{}'

	test2 := Test{
		optional_map: {
			'foo': 'bar'
		}
	}
	encoded2 := json2.encode(test2, escape_unicode: true)
	assert dump(encoded2) == '{"optional_map":{"foo":"bar"}}'
}
