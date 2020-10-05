fn test_ptr_assign() {
	v := [int(5), 6, 77, 1]
	unsafe {
		mut p := &v[0]
		(*p)++
		p++ // p now points to v[1]
		(*p) += 2
		p += 2 // p now points to v[3]
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
	assert q == unsafe {&v + 2}
}

struct S1 {
}

[unsafe]
fn (s S1) f() {
}

fn test_funcs() {
	s := S1{}
	unsafe {s.f()}
	_ = C.strerror(0) // [trusted] function prototype in builtin/cfns.c.v
}

fn test_if_expr_unsafe() {
	i := 4
	p := if true { unsafe {&i} } else { unsafe {&i} }
	assert *p == 4
}

fn test_unsafe_if_stmt() int {
	i := 4
	unsafe {
		if true {
			return (&i)[0]
		}
	}
	return i
}

fn test_map_address_index() {
	m := {
		'one': 1
	}
	unsafe {
		mut one := &m['one']
		one++
		println(*one)
	}
}
