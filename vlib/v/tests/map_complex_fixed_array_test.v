fn foo(mut m map[string][1][2]map[string]int) {
	m['foo'] = [[map{
		'bar': 1
	}, map{
		'baz': 3
	}]!]!
}

fn test_complex_map_fixed_array() {
	mut m := map[string][1][2]map[string]int{}
	foo(mut m)
	println(m)
	assert '$m' == "{'foo': [[{'bar': 1}, {'baz': 3}]]}"
}

fn test_innermost_value_of_map_fixed_array() {
	mut m := map[string][1][2]map[string]int{}
	m['foo'] = [[map{
		'bar': 1
	}, map{
		'baz': 3
	}]!]!
	println(m['foo'][0][0]['bar'])
	println(m['foo'][0][0]['bar'] == 1)
	assert m['foo'][0][0]['bar'] == 1
	assert '${m['foo'][0][0]['bar']}' == '1'
}

fn test_complex_map_high_order_fixed_array() {
	mut m := map{
		'foo': [[map{
			'a': 1
		}]!]!
		'bar': [[map{
			'b': 2
		}]!]!
	}
	for _, mut j in m {
		j = [[map{
			'c': 3
		}]!]!
	}
	println(m)
	assert '$m' == "{'foo': [[{'c': 3}]], 'bar': [[{'c': 3}]]}"
}
