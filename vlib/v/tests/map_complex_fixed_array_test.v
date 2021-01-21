fn foo(mut m map[string][1][2]map[string]int) {
	m['foo'] = [[{'bar': 1}, {'baz':3}]!]!
}

fn test_complex_map_fixed_array() {
	mut m := map[string][1][2]map[string]int
	foo(mut m)
	println(m)
	assert '$m' == "{'foo': [[{'bar': 1}, {'baz': 3}]]}"
}

fn test_innermost_value_of_map_fixed_array() {
	mut m := map[string][1][2]map[string]int
	m['foo'] = [[{'bar': 1}, {'baz': 3}]!]!
	println(m['foo'][0][0]['bar'])
    println(m['foo'][0][0]['bar'] == 1)
	assert m['foo'][0][0]['bar'] == 1
	assert '${m['foo'][0][0]['bar']}' == '1'
}
