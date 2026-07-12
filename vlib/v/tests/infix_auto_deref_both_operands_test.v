// Regression test for cgen auto-deref of BOTH operands in an infix expr.
//
// When both operands of an infix are auto-deref pointer vars - e.g. the two
// params of a sort lambda `|x, y| x < y` (both are `&T`), or the loop variable
// of a `for mut i in arr` used on both sides of an operator - each operand must
// be dereferenced. Previously the pointer-ness of one operand suppressed the
// dereference of the other, so `*x < *y` was emitted as the raw pointer
// comparison `x < y`, yielding unsorted output / wrong arithmetic.
// See: a.sorted(|x, y| x < y)

fn test_sorted_lambda_int_ascending() {
	a := [5, 2, 1, 9, 8]
	assert a.sorted(|x, y| x < y) == [1, 2, 5, 8, 9]
}

fn test_sorted_lambda_int_descending() {
	a := [5, 2, 1, 9, 8]
	assert a.sorted(|x, y| x > y) == [9, 8, 5, 2, 1]
}

fn test_sorted_lambda_matches_infix_form() {
	a := [5, 2, 1, 9, 8]
	// the lambda form must produce the same result as the classic `a < b` form
	assert a.sorted(|x, y| x < y) == a.sorted(a < b)
	assert a.sorted(|x, y| x > y) == a.sorted(a > b)
}

fn test_sorted_lambda_f64() {
	a := [3.5, 1.2, 9.9, 0.1]
	assert a.sorted(|x, y| x < y) == [0.1, 1.2, 3.5, 9.9]
}

fn test_sort_lambda_in_place() {
	mut a := [5, 2, 1, 9, 8]
	a.sort(|x, y| x < y)
	assert a == [1, 2, 5, 8, 9]
}

fn test_for_mut_infix_mul_both_operands() {
	mut ints := [1, 2, 3, 4]
	for mut i in ints {
		i = i * i
	}
	assert ints == [1, 4, 9, 16]
}

fn test_for_mut_infix_add_both_operands() {
	mut ints := [1, 2, 3]
	for mut i in ints {
		i = i + i
	}
	assert ints == [2, 4, 6]
}

fn test_for_mut_infix_comparison_both_operands() {
	mut flags := []bool{}
	mut nums := [10, 20, 30]
	for mut n in nums {
		// `n < n` is always false; the point is that both operands deref
		flags << (n < n)
		n = n - n
	}
	assert flags == [false, false, false]
	assert nums == [0, 0, 0]
}
