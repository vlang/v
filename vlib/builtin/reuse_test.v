fn test_buf_to_str() {
	s := 'abc'
	aview := unsafe { reuse_string_as_data(s) }
	dump(aview)
	assert aview == [u8(97), 98, 99]
	assert voidptr(aview.data) == voidptr(s.str)
	assert aview.len == s.len
}

fn test_str_to_buf() {
	a := [u8(88), 55, 77]
	sview := unsafe { reuse_data_as_string(a) }
	dump(sview)
	assert sview == 'X7M'
	assert voidptr(sview.str) == voidptr(a.data)
	assert sview.len == a.len
}
