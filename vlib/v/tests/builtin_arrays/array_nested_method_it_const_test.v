const it = 7
const nums = 7

fn const_grid() ?[][]int {
	return [[1, 2], [3]]
}

fn test_rebound_it_keeps_inner_it_with_a_constant_named_it() {
	assert it == 7
	outer := [10, 20]
	assert outer.all(if it := const_grid() { it.any(it == [1, 2]) } else { false })
	assert !outer.all(if it := const_grid() { it.all(it.len == 2) } else { false })
	assert outer.filter(if it := const_grid() { it.any(it.len == 1) } else { false }) == outer
}

fn test_rebound_nums_keeps_inner_it_with_a_constant_named_nums() {
	assert nums == 7
	outer := [10, 20]
	assert outer.all(if nums := const_grid() { nums.any(it == [1, 2]) } else { false })
}
