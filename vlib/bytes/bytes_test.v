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

fn test_split_empty_separator_preserves_remainder() {
	s := [u8(0), 0xff, 0xc3, 0xa9]
	assert split_n(s, []u8{}, 1) == [s]
	assert split_n(s, []u8{}, 2) == [[u8(0)], [u8(0xff), 0xc3, 0xa9]]
	assert split_n(s, []u8{}, 0) == [][]u8{}
	assert split_n(s, []u8{}, 10) == [[u8(0)], [u8(0xff)], [u8(0xc3)], [u8(0xa9)]]
	assert join(split(s, []u8{}), []u8{}) == s
	assert split([]u8{}, []u8{}) == [][]u8{}
	assert split([]u8{}, [u8(1)]) == [[]u8{}]
}

fn test_replace_empty_pattern_matches_byte_boundaries() {
	s := [u8(0xc3), 0xa9, 0xff]
	assert replace(s, []u8{}, [u8(0)], -1) == [u8(0), 0xc3, 0, 0xa9, 0, 0xff, 0]
	assert replace(s, []u8{}, [u8(0)], 2) == [u8(0), 0xc3, 0, 0xa9, 0xff]
	assert replace(s, []u8{}, [u8(0)], 0) == s
	assert replace([]u8{}, []u8{}, [u8(7)], -1) == [u8(7)]
	assert replace(s, []u8{}, []u8{}, -1) == s
}

fn test_raw_bytes_and_empty_inputs() {
	s := [u8(0), 0xff, 0xc3, 0xa9, `A`, `z`]
	assert count(s, []u8{}) == 7
	assert index(s, [u8(0xa9)]) == 3
	assert index(s, []u8{}) == 0
	assert contains([]u8{}, []u8{})
	assert has_prefix(s, []u8{})
	assert has_suffix(s, []u8{})
	assert to_lower(s) == [u8(0), 0xff, 0xc3, 0xa9, `a`, `z`]
	assert to_upper(s) == [u8(0), 0xff, 0xc3, 0xa9, `A`, `Z`]
	assert trim_space([u8(0xc2), 0xa0, ` `]) == [u8(0xc2), 0xa0]
	assert trim([u8(0xc3), 0xa9, 0xc3], [u8(0xc3)]) == [u8(0xa9)]
	assert repeat(s, -1) == []u8{}
	assert repeat([]u8{}, 100) == []u8{}
	assert repeat([]u8{}, 2147483647) == []u8{}
	assert join([][]u8{}, [u8(1)]) == []u8{}
	assert replace([u8(1), 1, 1], [u8(1), 1], [u8(2)], -1) == [u8(2), 1]
	assert count([u8(1), 1, 1], [u8(1), 1]) == 1
}

fn test_results_do_not_alias_inputs() {
	s := [u8(1), 2, 3]
	mut copies := [clone(s), join([s], []u8{}), repeat(s, 1), replace(s, [u8(9)], []u8{}, -1),
		replace(s, []u8{}, []u8{}, 0), to_lower(s), to_upper(s), trim_space(s), trim(s, []u8{}),
		trim_prefix(s, [u8(9)]), trim_suffix(s, [u8(9)])]
	for mut copy in copies {
		copy[0] = 9
		assert s == [u8(1), 2, 3]
	}
	mut parts := split_n(s, []u8{}, 2)
	parts[1][0] = 9
	assert s == [u8(1), 2, 3]
	mut separated := split(s, [u8(2)])
	separated[0][0] = 9
	assert s == [u8(1), 2, 3]
}
