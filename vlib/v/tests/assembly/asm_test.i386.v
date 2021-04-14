import v.tests.assembly.util
// rename this file to asm_test.amd64.v (and make more for other architectures) once pure v code is enforced

fn test_inline_asm() {
	a, mut b := 10, 0
	asm i386 {
		mov eax, a
		mov b, eax
		; +r (b)
		; r (a)
		; eax
	}
	assert a == 10
	assert b == 10

	mut c := 0
	asm i386 {
		mov c, 5
		; +r (c)
	}
	assert c == 5

	d, e, mut f := 10, 2, 0
	asm i386 {
		mov f, d
		add f, e
		add f, 5
		; +r (f) // output
		; r (d)
		  r (e) // input
	}
	assert d == 10
	assert e == 2
	assert f == 17

	// g, h, i := 2.3, 4.8, -3.5
	// asm rv64 {
	// 	fadd.s $i, $g, $h // test `.` in instruction name
	// 	: =r (i) as i
	// 	: r (g) as g
	// 	  r (g) as h
	// }
	// assert g == 2.3
	// assert h == 4.8
	// assert i == 7.1

	mut j := 0
	// do 5*3
	// adding three, five times
	asm i386 {
		mov ecx, 5 // loop 5 times
		loop_start:
		add j, 3
		loop loop_start
		; +r (j)
		; ; ecx
	}
	assert j == 5 * 3

	// k := 0 // Wait for tcc to implement goto, and gcc has odd errors
	// mut loops := 0
	// outside_label:
	// if k != 5 {
	// 	loops++
	// 	asm goto amd64 {
	// 		mov k, 1
	// 		mov k, 5
	// 		jmp outside_label
	// 		; =r (k) as k
	// 		; r (k)
	// 		;
	// 		; outside_label
	// 	}
	// }
	// assert loops == 1
	// assert k == 5

	// not marked as mut because we derefernce m to change l
	l := 5
	m := &l
	asm i386 {
		movd [m], 7 // have to specify size with q
		; ; r (m)
	}
	assert l == 7

	// same as above
	n := [5, 9, 0, 4]
	asm i386 {
		loop_start2:
		addd [in_data + ecx * 4 + 0], 2
		loop loop_start2
		addd [in_data + ecx * 4 + 0], 2
		; ; c (n.len - 1) // c is counter (loop) register
		  r (n.data) as in_data
	}
	assert n == [7, 11, 2, 6]

	mut manu := Manu{}
	asm amd64 {
		mov eax, 0
		cpuid
		; =b (manu.ebx) as ebx0
		  =d (manu.edx) as edx0
		  =c (manu.ecx) as ecx0
	}
	manu.str()
}

[packed]
struct Manu {
mut:
	ebx  u32
	edx  u32
	ecx  u32
	zero byte // for string
}

fn (m Manu) str() string {
	return unsafe {
		string{
			str: &byte(&m)
			len: 24
			is_lit: 1
		}
	}
}
