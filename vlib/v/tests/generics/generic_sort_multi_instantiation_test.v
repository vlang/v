// Regression test for vlang/v#27121: a generic sort comparator was reusing
// the body of an earlier instantiation, so calling `arrays.distinct` (or any
// generic that uses `.sort(a < b)` internally) on `[]int` and `[]string` in
// the same program produced a comparator that compared `int*` values via
// `builtin__string__lt`, failing C compilation.
import arrays

fn sort_helper[T](a []T) []T {
	mut x := a.clone()
	x.sort(a < b)
	return x
}

fn test_distinct_int_then_string() {
	assert arrays.distinct([3, 1, 2, 1, 3]) == [1, 2, 3]
	assert arrays.distinct(['c', 'a', 'b', 'a', 'c']) == ['a', 'b', 'c']
}

fn test_distinct_string_then_int() {
	assert arrays.distinct(['c', 'a', 'b', 'a', 'c']) == ['a', 'b', 'c']
	assert arrays.distinct([3, 1, 2, 1, 3]) == [1, 2, 3]
}

fn test_generic_sort_helper_reuse() {
	assert sort_helper([3, 1, 2]) == [1, 2, 3]
	assert sort_helper(['c', 'a', 'b']) == ['a', 'b', 'c']
	assert sort_helper([3.0, 1.0, 2.0]) == [1.0, 2.0, 3.0]
}
