struct Foo {
	sum fn (int, int) int
}

fn test_lambda_as_struct_field_value() {
	foo := Foo{
		sum: |x, y| x + y
	}
	assert foo.sum(6, 6) == 12
}
