type Foo = int | []int

fn works() bool {
	arr := [1, 2, 3]
	a := Foo(2)
	if a is int && unsafe { arr[..a] == [1, 2] } {
		println('works')
		return true
	}
	return false
}

fn test_if_expr_with_compound_conds() {
	assert works()
}
