interface Source {
	len int
}

fn test_array_as_interface() {
	arr := []rune{len: 1}
	src := Source(arr)
	assert arr.len == src.len
}
