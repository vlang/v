// vtest build: gcc
fn test_inline_asm() {
	a, mut b := 10, 0
	asm ppc64le {
		mr r12, a
		mr b, r12
		; +r (b)
		; r (a)
		; r12
	}
	assert a == b

	mut c := 0
	asm ppc64le {
		li c, 5
		; +r (c)
	}
	assert c == 5

	d, e, mut f := 10, 2, 0
	asm ppc64le {
		mr f, d
		add f, f, e
		addi f, f, 5
		; +r (f)
		; r (d)
		  r (e)
	}
	assert d == 10
	assert e == 2
	assert f == 17
}
