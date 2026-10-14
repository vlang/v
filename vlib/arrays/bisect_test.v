module arrays

fn test_bisect_with_duplicates() {
	a := [1, 2, 2, 2, 3]
	assert bisect_left(a, 2) == 1 && bisect_right(a, 2) == 4
}

fn test_bisect_floats() {
	f := [1.5, 2.5, 2.5, 3.5]
	assert bisect_left(f, 2.5) == 1
	assert bisect_right(f, 2.5) == 3
	assert bisect_left(f, 2.75) == 3
	assert bisect_right(f, 2.75) == 3
}

fn test_bisect_bounds() {
	empty := []int{}
	assert bisect_left(empty, 5) == 0
	assert bisect_right(empty, 5) == 0
	a := [10, 20, 30]
	assert bisect_left(a, 5) == 0, 'before first'
	assert bisect_right(a, 5) == 0
	assert bisect_left(a, 35) == 3, 'after last'
	assert bisect_right(a, 35) == 3
	assert bisect_left(a, 10) == 0
	assert bisect_right(a, 10) == 1
	assert bisect_left(a, 30) == 2
	assert bisect_right(a, 30) == 3
	single := [7]
	assert bisect_left(single, 7) == 0
	assert bisect_right(single, 7) == 1
	assert bisect_left(single, 6) == 0
	assert bisect_right(single, 8) == 1
}

fn test_bisect_left_right_differ_only_on_hits() {
	a := [1, 3, 3, 5]
	for v in [1, 3, 5] {
		assert bisect_left(a, v) < bisect_right(a, v), 'present values must differ'
	}
	for v in [0, 2, 4, 6] {
		assert bisect_left(a, v) == bisect_right(a, v), 'absent values must agree'
	}
}

fn test_bisect_strings() {
	mut a := ['apple', 'banana', 'cherry']
	assert bisect_left(a, 'banana') == 1
	assert bisect_right(a, 'banana') == 2
	assert bisect_left(a, 'blueberry') == 2, 'between banana and cherry'
	insort_right(mut a, 'blueberry')
	assert a == ['apple', 'banana', 'blueberry', 'cherry']
}

fn test_insort_keeps_order() {
	mut v := []int{}
	for x in [5, 3, 9, 1, 7, 3] {
		insort_left(mut v, x)
	}
	assert v == [1, 3, 3, 5, 7, 9]
	mut w := []int{}
	for x in [5, 3, 9, 1, 7, 3] {
		insort_right(mut w, x)
	}
	assert w == [1, 3, 3, 5, 7, 9]
	assert bisect_left(w, 3) == 1 && bisect_right(w, 3) == 3
}
