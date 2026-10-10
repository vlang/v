module arrays

// `carray_to_varray` reinterprets a raw pointer, so each call is wrapped in the
// smallest block that needs it.
fn copied_u8(data []u8, items int) []u8 {
	return unsafe { carray_to_varray[u8](&data[0], items) }
}

fn test_carray_to_varray_copies_u8_elements() {
	source := [u8(1), 2, 3, 4, 5]
	result := copied_u8(source, source.len)
	assert result == [u8(1), 2, 3, 4, 5]
}

fn test_carray_to_varray_copies_i32_elements() {
	source := [i32(-7), 0, 13, 1_000_000]
	result := unsafe { carray_to_varray[i32](&source[0], source.len) }
	assert result == [i32(-7), 0, 13, 1_000_000]
}

fn test_carray_to_varray_copies_f64_elements() {
	source := [f64(1.5), -2.25, 0.0]
	result := unsafe { carray_to_varray[f64](&source[0], source.len) }
	assert result == [f64(1.5), -2.25, 0.0]
}

fn test_carray_to_varray_with_zero_items_is_empty() {
	source := [u8(1), 2, 3]
	result := copied_u8(source, 0)
	assert result.len == 0
}

fn test_carray_to_varray_only_copies_the_requested_count() {
	source := [u8(1), 2, 3, 4, 5, 6]
	result := copied_u8(source[2..], 3)
	assert result == [u8(3), 4, 5]
}

fn test_carray_to_varray_preserves_repeated_bytes() {
	source := [u8(9), 9, 9, 9]
	result := copied_u8(source, source.len)
	assert result == [u8(9), 9, 9, 9]
}
