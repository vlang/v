fn test_ptr_arithmetic(){
	v := 4
	mut p := &v
	unsafe {
		p++
		p += 2
		p = p - 1
	}
	
	// byteptr, voidptr, charptr are handled differently
	mut q := byteptr(1)
	unsafe {
		q -= 2
		q = q + 1
	}
}
