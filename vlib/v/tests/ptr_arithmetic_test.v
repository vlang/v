fn test_ptr_arithmetic() {
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
	unsafe {
		q++
		q++
		q--
	}
	assert q == byteptr(10)
}

struct Abc {
mut:
	x int
	y int
	z int
}

fn test_ptr_arithmetic_over_struct() {
	mut a := [3]Abc{}
	a[0].x = 10
	a[1].x = 100
	a[2].x = 1000
	mut pa := &a[0]
	assert pa == &a[0]
	unsafe {
		assert pa.x == 10
		pa++
		assert u64(pa) - u64(&a[0]) == sizeof(Abc)
		assert pa.x == 100
		pa++
		assert u64(pa) - u64(&a[0]) == 2 * sizeof(Abc)
		assert pa.x == 1000
		pa--
		assert pa.x == 100
		pa--
		assert pa.x == 10
		pa += 2
		assert pa.x == 1000
		pa -= 2
	}
	assert pa == &a[0]
}
