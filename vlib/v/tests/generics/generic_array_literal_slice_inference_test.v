fn reduce_slices[T](items []T, combine fn (T, T) T) T {
	mut result := items[0]
	for item in items[1..] {
		result = combine(result, item)
	}
	return result
}

fn test_generic_array_literal_infers_slice_elements() {
	data := [u8(1), 2, 3, 4]
	middle := data[1..3]
	result := reduce_slices([data[..1], middle, data[3..]], fn (acc []u8, item []u8) []u8 {
		mut combined := acc.clone()
		combined << item
		return combined
	})
	assert result == data
}
