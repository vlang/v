module rand

// NOTE: temp until we have []bytes(buff)
fn new_array_from_c_array_no_alloc_rand(len, cap, elm_size int, c_array voidptr) array {
	arr := array {
		len: len
		cap: cap
		element_size: elm_size
		data: c_array
	}
	return arr
}
