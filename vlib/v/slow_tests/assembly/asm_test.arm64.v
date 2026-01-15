fn test_inline_asm() {
	a, mut b := 10, 0
	asm arm64 {
		mov x0, a
		mov b, x0
		; +r (b)
		; r (a)
		; x0
	}
	assert a == 10
	assert b == 10

	mut c := 0
	asm arm64 {
		mov c, 5
		; +r (c)
	}
	assert c == 5
	d, e, mut f := 10, 2, 0
	asm arm64 {
		mov x0, d
		mov x1, e
		add x0, x0, x1
		add x0, x0, 5
		mov f, x0
		; +r (f) // output
		; r (d)
		  r (e) // input
		; x0
		  x1
	}
	assert d == 10
	assert e == 2
	assert f == 17

	mut j := 0
	asm arm64 {
		mov x0, 5 // loop 5 times
		mov x1, 0
		loop_start:
		add x1, x1, 3
		sub x0, x0, 1
		cmp x0, 0
		b.gt loop_start
		mov j, x1
		; +r (j)
		; ; x0
		  x1
	}
	assert j == 5 * 3

	l := 5
	m := &l
	asm arm64 {
		mov w0, 7
		str w0, [m]
		; ; r (m)
		; w0
		  memory
	}
	assert l == 7
}

fn test_asm_generic() {
	u := u64(49)
	b := unsafe { bool(0) }
	assert generic_asm(u) == 14
	assert u == 63
	assert generic_asm(b) == true
	assert b == true
}

fn generic_asm[T](var &T) T {
	mut ret := T(14)
	unsafe {
		asm volatile arm64 {
			ldr x0, [var]
			add x0, x0, ret
			str x0, [var]
			; ; r (var)
			  r (ret)
			; x0
			  memory
		}
	}
	return ret
}
