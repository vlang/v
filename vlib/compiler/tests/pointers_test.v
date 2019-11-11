
fn test_pointer_arithmetic() {
	arr := [1,2,3,4]
	unsafe {
		mut parr := *int(arr.data)
		parr += 1
		assert 2 == *parr
	}
}

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

	assert n == 300 // updated by the unsafe pointer manipulation
}
