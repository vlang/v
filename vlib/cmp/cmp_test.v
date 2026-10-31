module cmp

fn test_compare() {
	assert compare(1, 2) == -1
	assert compare(2, 2) == 0
	assert compare(3, 2) == 1
	assert compare('a', 'b') == -1
	assert compare('x', 'x') == 0
}

fn test_less() {
	assert less(1, 2)
	assert !less(2, 2)
	assert !less(3, 2)
}

fn test_min_max() {
	assert min(3, 5) == 3
	assert max(3, 5) == 5
	assert min('a', 'b') == 'a'
	assert max('a', 'b') == 'b'
}

fn test_clamp() {
	assert clamp(5, 1, 10) == 5
	assert clamp(0, 1, 10) == 1
	assert clamp(20, 1, 10) == 10
}

fn test_coalesce() {
	assert coalesce(0, 0, 5) == 5
	assert coalesce(0, 0, 0) == 0
	assert coalesce('', '', 'x') == 'x'
	assert coalesce(false, true) == true
}
