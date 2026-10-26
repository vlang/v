module fastc

fn fastc_partition_test_cost(sizes []int, starts []int, first int, shared int) int {
	if starts.len == 0 { return -1 }
	mut largest := 0
	for group in 0 .. starts.len - 1 {
		mut size := if group == 0 { first } else { shared }
		for index in starts[group] .. starts[group + 1] { size += sizes[index] }
		if size > largest { largest = size }
	}
	return largest
}

fn fastc_assert_partition_matches_legacy(sizes []int, groups int, first int, shared int) {
	actual := fastc_unit_group_starts(sizes, groups, first, shared)
	legacy := fastc_legacy_unit_group_starts(sizes, groups, first, shared)
	assert actual == legacy, 'sizes=${sizes} groups=${groups} first=${first} shared=${shared}'
	assert fastc_partition_test_cost(sizes, actual, first, shared) == fastc_partition_test_cost(sizes, legacy, first, shared)
}

fn test_fastc_partition_search_preserves_zero_ties_and_overhead_fallbacks() {
	assert fastc_unit_group_starts([100, 200, 300, 400], 2, 0, 0) == [0, 3, 4]
	assert fastc_unit_group_starts([100, 200, 300, 400], 2, 250, 0) == [0, 2, 4]
	assert fastc_unit_group_starts([100, 100, 100, 100], 2, 0, 300) == [0, 3, 4]
	assert fastc_unit_group_starts([0, 0, 0, 0], 3, 5, 1) == [0, 1, 2, 4]
	// Increasing overheads invalidate the monotonicity proof and retain old DP.
	assert fastc_unit_group_starts([0, 1, 0, 0], 3, 0, 1) == [0, 2, 3, 4]
	for groups in [-1, 0, 1, 2, 3, 4, 5] {
		for sizes in [[]int{}, [0], [0, 0], [0, 1, 0, 0], [2, -1, 0, 3], [-2, 0, -1]] {
			for first in [-2, 0, 1, 5] {
				for shared in [-2, 0, 1, 5] {
					fastc_assert_partition_matches_legacy(sizes, groups, first, shared)
				}
			}
		}
	}
	// A cost equal to max_int is the legacy unreachable sentinel, not a split.
	assert fastc_unit_group_starts([0, 0], 2, max_int, 0) == []int{}
	for groups in 1 .. 3 {
		fastc_assert_partition_matches_legacy([max_int - 1, 0], groups, 0, 0)
		fastc_assert_partition_matches_legacy([max_int, 0], groups, 0, 0)
		fastc_assert_partition_matches_legacy([max_int - 2, 1], groups, 1, 0)
		fastc_assert_partition_matches_legacy([0, 1, 0], groups, max_int - 2, 0)
	}
}

fn test_fastc_partition_search_matches_legacy_exhaustively() {
	for length in 0 .. 6 {
		mut combinations := 1
		for _ in 0 .. length { combinations *= 3 }
		for pattern in 0 .. combinations {
			mut digits := pattern
			mut sizes := []int{len: length}
			for i in 0 .. length {
				sizes[i] = digits % 3
				digits /= 3
			}
			for first in 0 .. 5 {
				for shared in 0 .. 5 {
					for groups in 0 .. length + 2 {
						fastc_assert_partition_matches_legacy(sizes, groups, first, shared)
					}
				}
			}
		}
	}
}

fn fastc_legacy_unit_group_starts(unit_sizes []int, groups int, first_overhead int, shared_overhead int) []int {
	unit_count := unit_sizes.len
	if groups < 1 || unit_count < groups {
		return []int{}
	}
	mut prefix := []int{len: unit_count + 1}
	for i, size in unit_sizes {
		prefix[i + 1] = prefix[i] + size
	}
	mut previous := []int{len: unit_count + 1, init: max_int}
	previous[0] = 0
	mut splits := [][]int{len: groups + 1}
	for count in 1 .. groups + 1 {
		mut current := []int{len: unit_count + 1, init: max_int}
		splits[count] = []int{len: unit_count + 1, init: -1}
		overhead := if count == 1 { first_overhead } else { shared_overhead }
		for end in count .. unit_count + 1 {
			for start in count - 1 .. end {
				if previous[start] == max_int {
					continue
				}
				group_size := overhead + prefix[end] - prefix[start]
				largest := if previous[start] > group_size { previous[start] } else { group_size }
				if largest < current[end] {
					current[end] = largest
					splits[count][end] = start
				}
			}
		}
		previous = current.clone()
	}
	mut starts := []int{len: groups + 1}
	starts[groups] = unit_count
	mut end := unit_count
	for count := groups; count > 0; count-- {
		start := splits[count][end]
		if start < 0 {
			return []int{}
		}
		starts[count - 1] = start
		end = start
	}
	return starts
}
