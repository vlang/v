fn test_inline_asm() {
	a, b := 10, 0
	unsafe asm amd64 {
		mov eax, $1
		mov $0, eax
		: =r (b)
		: r (a)
		: rax
	}
	assert a == 10
	assert b == 10

	c := 0
	unsafe asm amd64 {
		mov $0, 5
		: =r (c)
		
	}
	assert c == 5

	d, e, f := 10, 2, 0
	unsafe asm amd64 {
		mov f, d
		add f, e
		add f, 5
		: =r (f) as f // output 
		: r (d) as d
		  r (e) as e // input 
	}
	assert d == 10
	assert e == 2
	assert f == 17

	// g, h, i := 2.3, 4.8, -3.5
	// unsafe asm rv64 {
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
	unsafe asm amd64 {
		mov j, $1
		mov rcx, 5 // loop 5 times
		loop_start:
		add j, 3
		loop loop_start
		: =r (j) as j
		: r (j)
		: rcx
	}
	assert j == 5 * 3

	// TODO goto
	// k := 0
	// loops := 0
	// outside_label:
	// if k != 5 {
	// 	loops++
	// 	unsafe asm goto amd64 {
	// 		mov $k, $1
	// 		mov $k, 5
	// 		jmp $outside_label
	// 		: =r (k) as k
	// 		: r (k)
	//		:
	//		: outside_label
	// 	}
	// }
	// assert loops == 0
	// assert k == 5

	l := 5
	m := &l
	unsafe asm amd64 {
		movq [m], 7 // have to specify size with q
		: : r (m) as m
	}
	assert l == 7

	n := [5, 9, 0, 4]
	unsafe asm amd64 {
		loop_start2:
		addq [in_data + rcx * 4 + 0], 2
		loop loop_start2
		addq [in_data + rcx * 4 + 0], 2
		: : c (n.len - 1) // c is counter (loop) register
		  r (n.data) as in_data
	}
	assert n == [7, 11, 2, 6]

	// m := `d`
	// unsafe asm amd64 {
	// 	cmp m, 0
	// 	je inif
	// 	mov m, `p`
	// 	jmp end
	// 	inif:
	// 	mov m, `l`
	// 	end:
	// 	: =r (m) as m
	// }	
	// assert m == `l`
}
