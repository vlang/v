fn foo(mut m map[string][1][2]map[string]int) {
	m['foo'] = [[{'bar': 1}, {'baz':3}]!]!
}

fn test_complex_map_fixed_array() {
	mut m := map[string][1][2]map[string]int
	foo(mut m)
	println(m)
	assert '$m' == "{'foo': [[{'bar': 1}, {'baz': 3}]]}"
}
