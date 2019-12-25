import arrays

fn test_range() {
	start_pos := 3
	end_pos := 10

	arr1 := arrays.range<int>(start_pos, end_pos)
	assert arr1.len == end_pos - start_pos
	for i, c in arr1 {
		assert c == i + start_pos
	}

	arr2 := arrays.range<f32>(start_pos, end_pos)
	assert arr2.len == end_pos - start_pos
	for i, c in arr2 {
		assert c == f32(i + start_pos)
	}

	arr3 := arrays.range<int>(start_pos, start_pos - 1)
	assert arr3.len == 0

	arr4 := arrays.range<int>(start_pos, start_pos)
	assert arr4.len == 0

	arr5 := arrays.range<int>(start_pos, start_pos + 1)
	assert arr5.len == 1
	assert arr5[0] == start_pos
}
