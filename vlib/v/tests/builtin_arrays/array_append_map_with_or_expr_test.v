fn test_array_append_map_with_or_expr() {
	foobar := {
		'foo': 'Foo'
		'bar': 'Bar'
	}
	mut arr := []string{}
	arr << foobar['foo']
	arr << foobar['baz'] or { 'Unknown' }
	assert arr == ['Foo', 'Unknown']
}
