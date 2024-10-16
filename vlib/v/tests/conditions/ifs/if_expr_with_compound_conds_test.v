type Foo = int | []int

fn test_if_expr_with_compound_conds() {
	a := Foo([3])
	if a is []int && a[0] == 3 {
		println('works')
	}
	assert true
}
