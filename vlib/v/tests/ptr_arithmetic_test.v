fn test_ptr_arithmetic(){
	v := 4
	mut p := &v
	unsafe {
		p++
		p += 2
		p = p - 1
	}
	p = unsafe { p + 1 }
	r := unsafe { p++ }
	p = r //unused
	
	// byteptr, voidptr, charptr are handled differently
	mut q := byteptr(1)
	unsafe {
		q -= 2
		q = q + 1
	}
	s := unsafe { q - 1 }
	q = s //unused
}
