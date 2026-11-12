fn test_inline_asm_rv64() {
	a, mut b := i64(123), i64(0)
	asm rv64 {
		// op dst, src
		mv t0, a
		mv b, t0
		; +r (b)
		; r (a)
		; t0
	}
	assert a == b

	mut c := 0
	asm rv64 {
		li c, 5
		; +r (c)
	}
	assert c == 5

	d, e, mut f := 10, 2, 0
	asm rv64 {
		mv f, d
		add f, f, e
		addi f, f, 5
		; +r (f)
		; r (d)
		  r (e)
	}
	assert d == 10
	assert e == 2
	assert f == 17

	g, h, mut i := 2.3, 4.8, -3.5
	asm rv64 {
		fadd.d i, g, h
		; =f (i)
		; f (g)
		  f (h)
	}
	assert g == 2.3
	assert h == 4.8
	assert i == 7.1

	n1, n2, mut sum, mut prod := 3, 5, -1, -1
	asm rv64 {
		add '%0', '%2', '%3'
		mul '%1', '%2', '%3'
		; =&r (sum)
		  =r (prod)
		; r (n1)
		  r (n2)
	}
	assert sum == 8
	assert prod == 15

	l := 5
	m := &l
	asm rv64 {
		li t0, 7
		sd t0, [m]
		; ; r (m)
		; memory
		  t0
	}
	assert l == 7
}

fn test_inline_asm_rv64_float_registers_and_memory_offsets() {
	values := [2.5, 4.5]!
	// The assembly reads this stack array before the function returns.
	ptr := unsafe { &values[0] }
	mut sum := 0.0
	asm riscv64 {
		fld f0, [ptr]
		fld f1, [ptr + 8]
		fadd.d sum, f0, f1
		; =f (sum)
		; r (ptr)
		; f0
		  f1
		  memory
	}
	assert sum == 7.0
}

fn riscv64_asm_goto_sign(value int) int {
	asm goto rv64 {
		beq value, zero, zero_value
		bgtz value, positive_value
		; ; r (value)
		; ; zero_value positive_value
	}
	return -1
	zero_value:
	return 0
	positive_value:
	return 1
}

fn riscv64_asm_goto_jump() int {
	asm goto riscv64 {
		j done
		; ; ; ; done
	}
	return 0
	done:
	return 1
}

fn test_inline_asm_rv64_goto_branches_and_fallthrough() {
	assert riscv64_asm_goto_sign(-7) == -1
	assert riscv64_asm_goto_sign(0) == 0
	assert riscv64_asm_goto_sign(7) == 1
	assert riscv64_asm_goto_jump() == 1
}
