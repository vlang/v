fn test_cstring() {
	w := &char(c'world')
	hlen := unsafe { C.strlen(c'hello') }
	hlen2 := unsafe { C.strlen('hello') }
	wlen := unsafe { C.strlen(w) }
	assert hlen == 5
	assert hlen2 == 5
	assert wlen == 5
}

fn test_cstring_with_zeros() {
	rawbytes := &char(c'\x00username\x00password')
	s := unsafe { rawbytes.vstring_with_len(18) }
	h := s.bytes().hex()
	assert h == '00757365726e616d650070617373776f7264'
}
