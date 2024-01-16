const base_map = {
	'a': 4
	'b': 5
}

fn test_map_init_with_update() {
	foo := {
		...base_map
		'b': 88
		'c': 99
	}
	assert base_map.keys() == ['a', 'b']
	assert base_map['a'] == 4
	assert base_map['b'] == 5
	assert foo.keys() == ['a', 'b', 'c']
	assert foo['a'] == 4
	assert foo['b'] == 88
	assert foo['c'] == 99

	bar := {
		...foo
		'b': 6
		'd': 7
	}
	assert base_map.keys() == ['a', 'b']
	assert base_map['a'] == 4
	assert base_map['b'] == 5
	assert foo.keys() == ['a', 'b', 'c']
	assert foo['a'] == 4
	assert foo['b'] == 88
	assert foo['c'] == 99
	assert bar.keys() == ['a', 'b', 'c']
	assert bar['a'] == 4
	assert bar['b'] == 6
	assert bar['d'] == 7
}
