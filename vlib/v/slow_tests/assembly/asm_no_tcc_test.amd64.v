// vtest build: !msvc && !tinyc
// The tests here have constraints, that are not implemented yet in tcc, and msvc does not support inline assembly at all
fn test_constraints() {
	x := u64(100)
	y := u64(200)
	mut hi := u64(0)
	mut lo := u64(0)
	asm amd64 {
		mov rax, x
		mulq y
		mov lo, rax
		mov hi, rdx
		; =*m (lo)
		  =&r (hi)
		; rm (x)
		  r (y)
		; rax
		  rdx
		  cc
	}
	assert hi == 0
	assert lo == 20000
}

fn test_flag_output() {
	a, b := 4, 9
	mut out := false
	asm amd64 {
		cmp a, b
		; =@ccl (out)
		; r (a)
		  r (b)
	}
	assert out
	asm amd64 {
		cmp b, a
		; =@ccl (out)
		; r (a)
		  r (b)
	}
	assert !out

	zero := 0
	asm amd64 {
		cmp zero, zero
		; =@ccz (out)
		; r (zero)
	}
	assert out

	mut maybe_four := 4
	mut four := 4
	asm amd64 {
		subl four, maybe_four
		testl four, maybe_four
		movl maybe_four, 9
		; +m (maybe_four)
		  +r (four)
		  =@ccz (out)
	}
	assert out
	assert maybe_four == 9
}
