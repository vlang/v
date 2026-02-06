fn test_sort_with_lambda_expr() {
	a := [5, 2, 1, 9, 8]
	dump(a)

	sorted01 := a.sorted(a < b)
	sorted02 := a.sorted(a > b)
	dump(sorted01)
	dump(sorted02)

	sorted01_with_compare_fn := a.sorted_with_compare(fn (a &int, b &int) int {
		return *a - *b
	})
	sorted02_with_compare_fn := a.sorted_with_compare(fn (a &int, b &int) int {
		return *b - *a
	})
	dump(sorted01_with_compare_fn)
	dump(sorted02_with_compare_fn)

	///////////////////////////////////////////

	sorted01_lambda_expr := a.sorted(|ix, iy| ix < iy)
	sorted02_lambda_expr := a.sorted(|ii, jj| ii > jj)
	dump(sorted01_lambda_expr)
	dump(sorted02_lambda_expr)

	sorted01_with_compare_lambda_expr := a.sorted_with_compare(|x, y| *x - *y)
	sorted02_with_compare_lambda_expr := a.sorted_with_compare(|e1, e2| *e2 - *e1)
	dump(sorted01_with_compare_lambda_expr)
	dump(sorted02_with_compare_lambda_expr)

	assert sorted01 == sorted01_with_compare_fn
	assert sorted02 == sorted02_with_compare_fn
	assert sorted01 == sorted01_lambda_expr
	assert sorted02 == sorted02_lambda_expr
	assert sorted01 == sorted01_with_compare_lambda_expr
	assert sorted02 == sorted02_with_compare_lambda_expr
}
