import json { decode }

fn test_json_decode_selected_array_type_argument() {
	values := decode([]string{}, '["one", "two"]') or { panic(err) }
	assert values == ['one', 'two']
}
