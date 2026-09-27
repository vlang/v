import json

fn test_json_array_type_argument() {
	values := json.decode([]string, '["one", "two"]') or { panic(err) }
	assert values == ['one', 'two']
	rows := json.decode([][]int, '[[1,2],[3]]') or { panic(err) }
	assert rows == [[1, 2], [3]]
}
