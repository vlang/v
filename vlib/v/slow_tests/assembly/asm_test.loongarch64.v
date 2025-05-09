// vtest build: gcc

asm loongarch64 {
	move r20, r21
}

fn test_inline_asm() {
	a, mut b := 10, 0
	asm loongarch64 {
		move r20, a
		move b, r20
		; +r (b)
		; r (a)
		; r20
	}
	assert a == b

	mut c := 0
	asm loongarch64 {
		li.w c, 5
		; +r (c)
	}
	assert c == 5

	d, e, mut f := 10, 2, 0
	asm loongarch64 {
		move f, d
		add.w f, f, e
		addi.w f, f, 5
		; +r (f)
		; r (d)
		  r (e)
	}
	assert d == 10
	assert e == 2
	assert f == 17
}
