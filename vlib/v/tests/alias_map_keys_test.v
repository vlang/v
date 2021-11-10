type Foo = map[string]string

fn test_alias_map_keys() {
	keys_list := Foo({
		'foo1': 'bar1'
		'foo2': 'bar2'
		'foo3': 'bar3'
	}).keys()

	println(keys_list)
	assert keys_list == ['foo1', 'foo2', 'foo3']
}
