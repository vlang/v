fn test_ptr_arithmetic(){
	unsafe {
		// Do NOT move this outside unsafe{}.
		// It causes too much churn in CI when new checks are implemented.
		// If you want to implement a specific failing test, do so inside
		// vlib/v/checker/tests/ , NOT here.
		v := 4
		mut p := &v
		p++
		p += 2
		p = p - 1
		assert p == &v + 2
		p = p + 1
		assert p == &v + 3
		r := p++
		assert r == &v + 3
		assert p == &v + 4
	}
}

fn test_ptr_arithmetic_over_byteptr() {	
	// byteptr, voidptr, charptr are handled differently
	mut q := byteptr(10)
	unsafe {
		q -= 2
		q = q + 1
	}
	assert q == byteptr(9)
	s := unsafe { q - 1 }
	assert s == byteptr(8)

	unsafe {q++ q++ q--}
	assert q == byteptr(10)
}
