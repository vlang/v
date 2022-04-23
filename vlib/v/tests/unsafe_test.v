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

fn test_double_ptr() {
	i := 5
	j := 7
	unsafe {
		mut x := &i
		mut p := &x
		(*p) = &j
		assert x == &j
	}
	// ///////
	mut x := &int(0)
	unsafe {
		mut p := &x
		(*p) = &int(1)
	}
	assert ptr_str(x) == ptr_str(&int(1))
}

fn test_ptr_infix() {
	v := 4
	mut q := unsafe { &v - 1 }
	q = unsafe { q + 3 }
	assert ptr_str(q) == ptr_str(unsafe { &v + 2 })
}

struct S1 {
}

[unsafe]
fn (s S1) f() {
}

fn test_funcs() {
	s := S1{}
	unsafe { s.f() }
	_ = C.strerror(0) // [trusted] function prototype in builtin/cfns.c.v
}

fn test_if_expr_unsafe() {
	i := 4
	ii := 123
	p := if true { unsafe { &i } } else { unsafe { &ii } }
	assert *p == 4
}

fn unsafe_if_stmt() int {
	i := 4
	unsafe {
		if true {
			return (&i)[0]
		}
	}
	return i
}

fn test_unsafe_if_stmt() {
	x := unsafe_if_stmt()
	assert x == 4
}

const fixedbytes = [100]u8{}

fn test_unsafe_pointers() {
	fsize := fixedbytes.len
	src := &fixedbytes[0]
	//
	b := []u8{}
	eprintln('b.data before: $b.data')
	eprintln('b.len before: $b.len')
	eprintln('b.cap before: $b.cap')
	assert b.len == 0
	unsafe {
		// here b will be setup to work with the mmaped region
		mut pdata := &b.data
		mut plen := &b.len
		mut pcap := &b.cap
		// note that pdata, plen, pcap are used here:
		*pdata = src
		*plen = int(fsize)
		*pcap = int(fsize)
	}
	assert b.len == 100
	assert b.cap == 100
	assert b.data == src
	eprintln('b.data after: $b.data')
	eprintln('b.len after: $b.len')
	eprintln('b.cap after: $b.cap')
}
