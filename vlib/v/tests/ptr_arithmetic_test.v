fn test_ptr_arithmetic() {
	mut v := 4
	mut p := &v
	unsafe {
		p++
		p += 2
	}
	p = unsafe(p - 1)
	assert p == unsafe(&v + 2)
	p = unsafe(p + 1)
	assert p == unsafe(&v + 3)
	r := unsafe(p++)
	assert r == unsafe(&v + 3)
	assert p == unsafe(&v + 4)
}

fn test_ptr_arithmetic_over_byteptr() {
	// byteptr, voidptr, charptr are handled differently
	mut q := byteptr(10)
	unsafe {
		q -= 2
		q = q + 1
	}
	assert q == byteptr(9)
	s := unsafe(q - 1)
	assert s == byteptr(8)
}
