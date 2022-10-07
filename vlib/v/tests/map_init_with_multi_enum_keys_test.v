enum Foo {
	a
	b
}

fn test_map_init_with_multi_enum_keys() {
	mp := {
		Foo.a: 'A'
		.b:    'B',
	}
	println(mp)
	assert mp[.a] == 'A'
	assert mp[.b] == 'B'
}
