struct SortLambdaPoint {
	x int
	y int
}

fn sort_lambda_generic[T](mut xs []T) {
	xs.sort_with_compare(|a, b| if *a < *b {
		-1
	} else if *a > *b {
		1
	} else {
		0
	})
}

fn test_sort_with_compare_lambda_derefs_arguments() {
	mut ps := [3, 1, 2]
	ps.sort_with_compare(|a, b| *a - *b)
	assert ps == [1, 2, 3]
}

fn test_sort_with_compare_lambda_captures_outer_local() {
	w := [5, 1, 3]
	mut ps := [0, 1, 2]
	ps.sort_with_compare(|a, b| w[*a] - w[*b])
	assert ps == [1, 2, 0]
}

fn test_sort_with_compare_lambda_on_struct_fields() {
	mut ps := [SortLambdaPoint{3, 1}, SortLambdaPoint{1, 2}, SortLambdaPoint{2, 0}]
	ps.sort_with_compare(|a, b| a.x - b.x)
	assert ps.map(it.x) == [1, 2, 3]
	sorted := ps.sorted_with_compare(|a, b| a.y - b.y)
	assert sorted.map(it.y) == [0, 1, 2]
}

fn test_sort_with_compare_lambda_with_if_and_match_body() {
	mut ps := [SortLambdaPoint{1, 3}, SortLambdaPoint{0, 5}, SortLambdaPoint{1, 2}]
	ps.sort_with_compare(|a, b| if a.x != b.x { a.x - b.x } else { a.y - b.y })
	assert ps == [SortLambdaPoint{0, 5}, SortLambdaPoint{1, 2}, SortLambdaPoint{1, 3}]
	mut ns := [3, 1, 2]
	ns.sort_with_compare(|a, b| match true {
		*a < *b { 1 }
		*a > *b { -1 }
		else { 0 }
	})
	assert ns == [3, 2, 1]
}

fn test_sort_lambda_with_if_body() {
	mut ps := [SortLambdaPoint{1, 3}, SortLambdaPoint{0, 5}, SortLambdaPoint{1, 2}]
	ps.sort(|a, b| if a.x != b.x { a.x < b.x } else { a.y < b.y })
	assert ps == [SortLambdaPoint{0, 5}, SortLambdaPoint{1, 2}, SortLambdaPoint{1, 3}]
}

fn test_sort_with_compare_lambda_in_generic_fn() {
	mut ints := [5, 2, 9]
	sort_lambda_generic(mut ints)
	assert ints == [2, 5, 9]
	mut floats := [3.5, 1.5]
	sort_lambda_generic(mut floats)
	assert floats == [1.5, 3.5]
}
