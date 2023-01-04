struct Foo {
	bar int
}

fn test_if_expr_with_struct_init() {
	a := Foo{}
	if a == Foo{} {
		println(true)
		assert true
	}
}
