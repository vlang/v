fn test_ptr_assign() {
	v := [int(5), 6, 77, 1]
	mut p := &v[0]
	unsafe {
		(*p)++
	}
	unsafe {
		p++
	} // p now points to v[1]
	unsafe {
		(*p) += 2
	}
	unsafe {
		p += 2
	} // p now points to v[3]
	unsafe {
		*p = 31
	}
	assert v[0] == 6
	assert v[1] == 8
	assert v[2] == 77
	assert v[3] == 31
}

fn test_ptr_infix() {
	v := 4
	mut q := unsafe {&v - 1}
	q = unsafe {q + 3}
	_ := q
	_ := v
}

struct S1 {
}

[unsafe]
fn (s S1) f() {
}

fn test_funcs() {
	s := S1{}
	unsafe {
		s.f()
	}
	_ = C.strerror(0) // [trusted] function prototype in builtin/cfns.c.v
}
