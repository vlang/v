import v.slow_tests.assembly.util

fn test_inline_asm() {
	a, mut b := i64(10), i64(0)
	asm amd64 {
		mov rax, a
		mov b, rax
		; +r (b)
		; r (a)
		; rax
	}
	assert a == 10
	assert b == 10

	mut c := 0
	asm amd64 {
		mov c, 5
		; +r (c)
	}
	assert c == 5

	d, e, mut f := 10, 2, 0
	asm amd64 {
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
	asm amd64 {
		mov rcx, 5 // loop 5 times
		loop_start:
		add j, 3
		loop loop_start
		; +r (j)
		; ; rcx
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

	// not marked as mut because we dereference m to change l
	l := 5
	m := &l
	asm amd64 {
		movq [m], 7 // have to specify size with q
		; ; r (m)
		; memory
	}
	assert l == 7

	// same as above
	n := [5, 9, 0, 4]
	asm amd64 {
		loop_start2:
		addq [in_data + rcx * 4 + 0], 2
		loop loop_start2
		addq [in_data + rcx * 4 + 0], 2
		; ; c (n.len - 1) // c is counter (loop) register
		  r (n.data) as in_data
		; memory
	}
	assert n == [7, 11, 2, 6]

	assert util.add(8, 9, 34, 7) == 58 // test .amd64.v imported files

	mut o := Manu{}
	asm amd64 {
		mov eax, 0
		cpuid
		; =b (o.ebx) as ebx0
		  =d (o.edx) as edx0
		  =c (o.ecx) as ecx0
	}
	o.str()
}

@[packed]
struct Manu {
mut:
	ebx  u32
	edx  u32
	ecx  u32
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

// this test does not appear in i386 test since rip relative addressing was introduced in 64-bit mode
// doesn't actually work
@[if !macos]
fn test_rip_relative_label() {
	$if !macos {
		mut a := i64(4)
		asm amd64 {
			mov a, [rip + one_two_three] // see below
			; =r (a)
		}
		assert a == 48321074923
	}
}

$if !macos {
	asm amd64 {
		.global one_two_three
		one_two_three:
		.quad 48321074923
	}
}

// this test does not appear in i386 test since rip relative addressing was introduced in 64-bit mode
// doesn't actually work
@[if !macos]
fn test_rip_relative_label_u8() {
	$if !macos {
		mut a := int(4)
		asm amd64 {
			mov a, [rip + byte_sequence] // see below
			; =r (a)
		}
		assert a == 0x480f3527
	}
}

$if !macos {
	asm amd64 {
		.global byte_sequence
		byte_sequence:
		.byte 0x27, 0x35, 0x0f, 0x48
	}
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

fn test_asm_generic() {
	u := u64(49)
	assert generic_asm(u) == 14
	assert u == 63

	i := i32(123)
	assert generic_asm(i) == 14
	assert i == 137
}

fn generic_asm[T](var &T) T {
	mut ret := T(14)
	unsafe {
		asm volatile amd64 {
			add var, ret
			; +m (var[0]) as var
			  +r (ret)
			; ; memory
		}
	}
	return ret
}

fn test_lock_prefix() {
	mut rv := u64(123)
	mut atom := u64(0)
	cmp := u64(1)
	xchg := u64(1)

	asm amd64 {
		lock cmpxchgq '%1', '%2'
		; =a (rv)
		  +m (atom)
		; q (xchg)
		  0 (cmp)
		; memory
	}
	assert rv == 0
	assert atom == 0
}
