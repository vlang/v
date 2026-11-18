struct RangeBounds {
mut:
	start int
	calls int
}

fn (mut bounds RangeBounds) limit() int {
	bounds.calls++
	bounds.start = 3
	return 4
}

fn test_for_in_reads_lower_bound_before_upper_bound_call() {
	mut bounds := RangeBounds{ start: 1 }
	mut seen := []int{}
	for i in bounds.start .. bounds.limit() {
		seen << i
	}
	assert seen == [1, 2, 3]
	assert bounds.calls == 1
}

fn test_for_in_reads_lower_bound_before_compound_upper_bound() {
	mut bounds := RangeBounds{ start: 1 }
	mut seen := []int{}
	for i in bounds.start .. (1 + bounds.limit()) {
		seen << i
	}
	assert seen == [1, 2, 3, 4]
	assert bounds.calls == 1
}
