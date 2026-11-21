fn test_for_in_array_slice_captures_start() {
	values := [1, 2, 3]
	mut seen := []int{}
	for value in values[seen.len..] {
		seen << value
	}
	assert seen == values
}

fn test_for_in_array_slice_captures_end() {
	values := [1, 2, 3, 4]
	mut seen := []int{}
	mut end := 3
	for index, value in (values[1..end]) {
		assert index == seen.len
		seen << value
		end--
	}
	assert seen == [2, 3]
}

fn test_for_in_mut_array_slice_keeps_backing_storage() {
	mut values := [1, 2, 3, 4]
	mut end := 3
	for mut value in values[1..end] {
		value *= 10
		end--
	}
	assert values == [1, 20, 30, 4]
}

fn test_for_in_string_slice_captures_end() {
	text := 'abcd'
	mut end := 3
	mut seen := []u8{}
	for byte in text[1..end] {
		seen << byte
		end--
	}
	assert seen == [u8(`b`), u8(`c`)]
}
