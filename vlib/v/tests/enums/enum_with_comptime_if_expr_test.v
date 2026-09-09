const expected_foo_value = $if macos { 1 } $else { 0 }

enum Foo {
	foo = $if macos { 1 } $else { 0 }
	bar
}

fn test_enum_values_from_comptime_if_expr() {
	assert int(Foo.foo) == expected_foo_value
	assert int(Foo.bar) == expected_foo_value + 1
}
