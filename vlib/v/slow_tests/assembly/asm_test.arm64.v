// vtest build: gcc
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
		mov x0, 5
		mov c, x0
		; +r (c)
		; ; x0
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
	// do 5*3
	// adding three, five times
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
	/*
	// not marked as mut because we dereference m to change l
	l := 5
	m := &l
	asm arm64 {
		movd [m], 7 // have to specify size with q
		; ; r (m)
	}
	assert l == 7

	mut manu := Manu{}
	asm arm64 {
		mov x0, MIDR_EL1
		mov x1, ID_AA64ISAR0_EL1
		mov x2, ID_AA64MMFR0_EL1
		; =r (manu.midr_el1) as x0
		  =r (manu.id_aa64isar0_el1) as x1
		  =r (manu.id_aa64mmfr0_el1) as x2
	}
	manu.str()
	*/
}

/*
@[packed]
struct Manu {
mut:
	midr_el1  u64
	id_aa64isar0_el1  u64
	id_aa64mmfr0_el1  u64
	zero u8 // for string
}

fn (m Manu) str() string {
	return unsafe {
		string{
			str:    &u8(&m)
			len:    24
			is_lit: 1
		}
	}
}

fn test_asm_generic() {
	u := u64(49)
	b := unsafe { bool(123) }
	assert generic_asm(u) == 14
	assert u == 63
	assert u64(generic_asm(b)) == 14
	assert u64(b) == 137
}

fn generic_asm[T](var &T) T {
	mut ret := T(14)
	unsafe {
		asm volatile arm64 {
			mov x0, ret
			mov x1, var
			add x1, x0, 0
			mov var, x1
			; +m (var[0]) as var
			  +r (ret)
			; ; memory x0 x1
		}
	}
	return ret
}
*/
