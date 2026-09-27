interface MapLength {
	len int
}

fn test_map_header_is_pointer_sized() {
	assert sizeof(map[int]int) == sizeof(voidptr)
}

fn test_map_len_interface_uses_map_data() {
	m := {
		'one': 1
		'two': 2
	}
	length := MapLength(m)
	assert length.len == 2
}
