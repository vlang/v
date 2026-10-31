fn empty_list_of[T](_ T) []T {
	return []T{}
}

fn list_of[T](_ T) []T {
	return []T{len: 2}
}

fn fixed_of[T](_ T) [3]T {
	return [3]T{}
}

// `[]T{}` stays a dynamic array when `T` is a fixed array.
fn test_dynamic_array_init_with_fixed_array_element_type() {
	empty := empty_list_of([7]u8{})
	assert empty.len == 0
	list := list_of([4]int{})
	assert list.len == 2
	assert list[1].len == 4
	fixed := fixed_of([2]u8{})
	assert fixed.len == 3
	assert fixed[0].len == 2
}
