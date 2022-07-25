struct Foo {
	text string = $if linux {
		'linux'
	} $else {
		println('else')
		'else'
	}
}

fn test_comptime_if_expr_in_struct_field_default() {
	f := Foo{}
	println(f)
	assert f.text.len > 0
}
