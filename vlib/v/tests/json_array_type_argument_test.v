import json2

fn test_json_array_type_argument() {
	values := json2.decode[[]string]('["one", "two"]') or { panic(err) }
	assert values == ['one', 'two']
	rows := json2.decode[[][]int]('[[1,2],[3]]') or { panic(err) }
	assert rows == [[1, 2], [3]]
}
