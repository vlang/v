fn test_pointer_arithmetic() {
	arr := [1, 2, 3, 4]
	unsafe {
		mut parr := &int(arr.data)
		assert 1 == *parr
		parr++
		assert 2 == *parr
		parr++
		assert 3 == *parr
		assert *(parr + 1) == 4
	}
}

/*
fn test_multi_level_pointer_dereferencing() {
	n := 100
	pn := &n
	ppn := &pn
	unsafe {
		mut pppn := &ppn
		***pppn = 300
		pppa := ***int(pppn)
		assert 300 == ***pppa
	}
	assert n == 300	// updated by the unsafe pointer manipulation
}
*/
