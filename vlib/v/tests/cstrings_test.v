fn test_cstring() {
	w := c'world'
	hlen := C.strlen(c'hello')
	hlen2 := C.strlen('hello')
	wlen := C.strlen(w)
	assert hlen == 5
	assert hlen2 == 5
	assert wlen == 5
}
