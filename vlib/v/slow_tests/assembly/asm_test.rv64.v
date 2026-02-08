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
