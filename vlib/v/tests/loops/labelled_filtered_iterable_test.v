fn test_labelled_filter_continue() {
	values := [0, 1, 2, 3]
	mut seen := []int{}
	outer: for value in values.filter(it > 0) {
		for _ in 0 .. 2 {
			if value == 2 { continue outer }
			seen << value
			break
		}
	}
	assert seen == [1, 3]
}

fn test_labelled_filter_map_break() {
	values := [0, 1, 2, 3]
	mut seen := []int{}
	outer: for value in values.filter(it > 0).map(it * 2) {
		for _ in 0 .. 2 {
			if value == 4 { break outer }
			seen << value
		}
	}
	assert seen == [2, 2]
}

fn test_unused_label_on_filtered_iterable() {
	values := [0, 1, 2]
	mut seen := []int{}
	outer: for value in values.filter(it > 0) { seen << value }
	assert seen == [1, 2]
}
