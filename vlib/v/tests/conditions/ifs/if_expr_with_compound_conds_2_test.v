type Foo = int | []int

fn works() bool {
	arr := [1, 2, 3]
	a := Foo(2)
	if a is int && arr[a] == 3 {
		println('works')
		return true
	}
	return false
}

fn test_if_expr_with_compound_conds() {
	assert works()
}
