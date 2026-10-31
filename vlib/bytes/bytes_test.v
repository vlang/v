module bytes

fn test_compare_equal() {
	assert compare([u8(1), 2], [u8(1), 2]) == 0
	assert compare([u8(1)], [u8(2)]) == -1
	assert compare([u8(2)], [u8(1)]) == 1
	assert compare([u8(1)], [u8(1), 2]) == -1
}

fn test_equal_contains() {
	assert equal([u8(1), 2], [u8(1), 2])
	assert !equal([u8(1)], [u8(2)])
	assert contains([u8(1), 2, 3], [u8(2), 3])
	assert !contains([u8(1), 2], [u8(3)])
}

fn test_count() {
	assert count([u8(1), 2, 1, 2], [u8(1), 2]) == 2
	assert count([u8(1), 2, 3], [u8(9)]) == 0
	assert count([u8(1), 2], []u8{}) == 3
}

fn test_prefix_suffix_index() {
	assert has_prefix([u8(1), 2, 3], [u8(1), 2])
	assert !has_prefix([u8(1), 2, 3], [u8(2)])
	assert has_suffix([u8(1), 2, 3], [u8(2), 3])
	assert index([u8(1), 2, 3, 2], [u8(2)]) == 1
	assert index_byte([u8(1), 2, 3], 3) == 2
	assert index([u8(1), 2], [u8(9)]) == -1
}

fn test_join_repeat() {
	assert join([[u8(1)], [u8(2), 3]], [u8(0)]) == [u8(1), 0, 2, 3]
	assert repeat([u8(1), 2], 2) == [u8(1), 2, 1, 2]
	assert repeat([u8(1)], 0) == []u8{}
}

fn test_replace() {
	assert replace([u8(1), 2, 1], [u8(1)], [u8(9)], -1) == [u8(9), 2, 9]
	assert replace([u8(1), 2, 1], [u8(1)], [u8(9)], 1) == [u8(9), 2, 1]
}

fn test_split() {
	assert split([u8(1), 0, 2, 0, 3], [u8(0)]) == [[u8(1)], [u8(2)], [u8(3)]]
	n := split_n([u8(1), 0, 2, 0, 3], [u8(0)], 2)
	assert n == [[u8(1)], [u8(2), 0, 3]]
}

fn test_case_trim() {
	assert to_lower([u8(65), 98]) == [u8(97), 98]
	assert to_upper([u8(97), 66]) == [u8(65), 66]
	assert trim_space([u8(32), 65, 32]) == [u8(65)]
	assert trim([u8(1), 2, 3, 1], [u8(1)]) == [u8(2), 3]
	assert trim_prefix([u8(1), 2, 3], [u8(1)]) == [u8(2), 3]
	assert trim_suffix([u8(1), 2, 3], [u8(3)]) == [u8(1), 2]
}

fn test_clone() {
	a := [u8(1), 2]
	b := clone(a)
	assert equal(a, b)
}
