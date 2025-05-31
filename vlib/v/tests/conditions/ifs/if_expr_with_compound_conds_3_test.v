type Foo = int | []int

fn works() bool {
	a := Foo(2)
	if a is int && '${a}' == '2' {
		println('works')
		return true
	}
	return false
}

fn test_if_expr_with_compound_conds() {
	assert works()
}
