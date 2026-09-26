struct Point {
	x int
}

fn cmp_ints(a &int, b &int) int {
	return *a - *b
}

fn sorted_returned(a []int) []int {
	return a.sorted(a < b)
}

fn sorted_returned_lambda(a []int) []int {
	return a.sorted(|x, y| x > y)
}

fn sorted_returned_by_field(a []Point) []Point {
	return a.sorted(a.x > b.x)
}

fn sorted_returned_generic[T](a []T) []T {
	return a.sorted(a < b)
}

fn first_of(a []int) int {
	return a[0]
}

fn maybe(n int) ?int {
	return if n > 0 { n } else { none }
}

fn test_sorted_as_return_value() {
	assert sorted_returned([3, 1, 2]) == [1, 2, 3]
	assert sorted_returned_lambda([3, 1, 2]) == [3, 2, 1]
	assert sorted_returned_by_field([Point{1}, Point{3}, Point{2}]) == [
		Point{3},
		Point{2},
		Point{1},
	]
	assert sorted_returned_generic(['b', 'c', 'a']) == ['a', 'b', 'c']
	assert sorted_returned_generic([2.5, 0.5, 1.5]) == [0.5, 1.5, 2.5]
}

fn test_sorted_as_call_argument() {
	a := [5, 2, 1, 9, 8]
	assert first_of(a.sorted(a > b)) == 9
	assert first_of(a.sorted_with_compare(cmp_ints)) == 1
	// A temporary queued earlier in the same statement must stay outside the sort.
	assert (maybe(1) or { 0 }) + first_of(a.sorted(a < b)) == 2
}

fn test_sorted_with_compare_lambda_deref() {
	a := [5, 2, 1, 9, 8]
	assert a.sorted_with_compare(|x, y| *x - *y) == [1, 2, 5, 8, 9]
	assert first_of(a.sorted_with_compare(|x, y| *y - *x)) == 9
	mut b := a.clone()
	b.sort_with_compare(|x, y| *x - *y)
	assert b == [1, 2, 5, 8, 9]
}
