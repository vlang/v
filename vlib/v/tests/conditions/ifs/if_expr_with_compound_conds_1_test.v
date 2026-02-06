type Foo = int | []int

fn works() bool {
	a := Foo([3])
	if a is []int && a[0] == 3 {
		println('works')
		return true
	}
	return false
}

fn test_if_expr_with_compound_conds() {
	assert works()
}
