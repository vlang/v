enum Foo {
	a
	b
}

type NestedAbc = map[string]string | string

enum NestedFoo {
	a
	b
	c
}

fn test_map_init_with_multi_enum_keys() {
	mp := {
		Foo.a: 'A'
		.b:    'B'
	}
	println(mp)
	assert mp[.a] == 'A'
	assert mp[.b] == 'B'
}

fn test_nested_map_init_with_multi_enum_keys() {
	mp := {
		NestedFoo.a: NestedAbc({
			'A': 'AA'
		})
		.b:          'B'
		.c:          {
			'c': 'C'
		}
	}
	println(mp)
	assert mp[.a]! == NestedAbc({
		'A': 'AA'
	})
	assert mp[.b]! == NestedAbc('B')
	assert mp[.c]! == NestedAbc({
		'c': 'C'
	})
}
