interface Source {
	element_size int
	data voidptr
	offset int
	len int
	cap int
	flags ArrayFlags
}

fn test_array_as_interface() {
	arr := []rune{len: 1}
	src := Source(arr)
	assert arr.element_size == src.element_size
	assert arr.data == src.data
	assert arr.offset == src.offset
	assert arr.len == src.len
	assert arr.cap == src.cap
	assert arr.flags == src.flags
}
