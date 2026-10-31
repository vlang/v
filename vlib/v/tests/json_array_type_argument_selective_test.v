import json2

fn test_json_decode_selected_array_type_argument() {
	values := json2.decode[[]string{}]('["one", "two"]') or { panic(err) }
	assert values == ['one', 'two']
}
