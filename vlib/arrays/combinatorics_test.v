import arrays

fn sorted_copy(a []int) []int {
	mut c := a.clone()
	c.sort()
	return c
}

fn test_permutations_count_and_content() {
	res := arrays.permutations([1, 2, 3])
	assert res.len == 6
	mut seen := map[string]bool{}
	for row in res {
		assert sorted_copy(row) == [1, 2, 3], 'every row must permute the input'
		seen[row.str()] = true
	}
	assert seen.len == 6, 'all permutations must be distinct'
}

fn test_permutations_edge_cases() {
	empty := arrays.permutations([]int{})
	assert empty.len == 1 && empty[0].len == 0, 'empty input yields one empty row'
	single := arrays.permutations(['x'])
	assert single.len == 1 && single[0] == ['x']
}

fn test_combinations_exact_order() {
	res := arrays.combinations([1, 2, 3, 4], 2)
	assert res.len == 6
	assert res[0] == [1, 2]
	assert res[2] == [1, 4]
	assert res[5] == [3, 4]
	full := arrays.combinations([1, 2], 2)
	assert full.len == 1 && full[0] == [1, 2]
	assert arrays.combinations([1, 2, 3], 0).len == 1
	assert arrays.combinations([1, 2], 3).len == 0
	assert arrays.combinations([1, 2], -1).len == 0
}

fn test_product() {
	res := arrays.product([1, 2], [10, 20])
	assert res.len == 4
	assert res[0] == [1, 10]
	assert res[1] == [1, 20]
	assert res[2] == [2, 10]
	assert res[3] == [2, 20]
	assert arrays.product([]int{}, [1]).len == 0
	assert arrays.product([1], []int{}).len == 0
}
