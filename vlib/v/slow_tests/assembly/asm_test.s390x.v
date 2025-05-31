// vtest build: gcc
fn test_inline_asm() {
	a, mut b := 10, 0
	asm s390x {
		lgr r2, a
		lgr b, r2
		; +r (b)
		; r (a)
		; r2
	}
	assert a == b

	mut c := 0
	asm s390x {
		lgfi c, 5
		; +r (c)
	}
	assert c == 5

	d, e, mut f := 10, 2, 0
	asm s390x {
		lgr f, d
		ar f, e
		ahi f, 5
		; +r (f)
		; r (d)
		  r (e)
	}
	assert d == 10
	assert e == 2
	assert f == 17
}
