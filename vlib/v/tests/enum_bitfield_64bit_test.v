// TODO: fix the formatting here, when VFMT ON/OFF is working

[flag]
enum PawnsBoard as u64 {
	a8
	b8
	c8
	d8
	e8
	f8
	g8
	h8
	a7
	b7
	c7
	d7
	e7
	f7
	g7
	h7
	a6
	b6
	c6
	d6
	e6
	f6
	g6
	h6
	a5
	b5
	c5
	d5
	e5
	f5
	g5
	h5
	a4
	b4
	c4
	d4
	e4
	f4
	g4
	h4
	a3
	b3
	c3
	d3
	e3
	f3
	g3
	h3
	a2
	b2
	c2
	d2
	e2
	f2
	g2
	h2
	a1
	b1
	c1
	d1
	e1
	f1
	g1
	h1
}

fn test_flag_enum_with_64_value_bits() {
	last_value := PawnsBoard.h1
	dump(u64(last_value))
	assert u64(last_value) == 0x8000_0000_0000_0000

	wb := PawnsBoard.a2 | .b2 | .c2 | .d2 | .e2 | .f2 | .g2 | .h2
	dump(wb)
	dump(u64(wb))
	assert u64(wb) == 71776119061217280
	assert '${u64(wb):064b}' == '0000000011111111000000000000000000000000000000000000000000000000'
	assert '$wb' == 'PawnsBoard{.a2 | .b2 | .c2 | .d2 | .e2 | .f2 | .g2 | .h2}'

	bb := PawnsBoard.a7 | .b7 | .c7 | .d7 | .e7 | .f7 | .g7 | .h7
	dump(bb)
	dump(u64(bb))
	assert u64(bb) == 65280
	assert '${u64(bb):064b}' == '0000000000000000000000000000000000000000000000001111111100000000'
	assert '$bb' == 'PawnsBoard{.a7 | .b7 | .c7 | .d7 | .e7 | .f7 | .g7 | .h7}'

	if false {
		eprintln('----------------------------------------------')
		for x in [PawnsBoard.a1, .a2, .a3, .a4, .a5, .a6, .a7, .a8] {
			eprintln('>> x: $x | hex value: ${u64(x).hex()}')
		}
		eprintln('----------------------------------------------')
		for x in [PawnsBoard.b1, .b2, .b3, .b4, .b5, .b6, .b7, .b8] {
			eprintln('>> x: $x | hex value: ${u64(x).hex()}')
		}
		eprintln('----------------------------------------------')
		for x in [PawnsBoard.c1, .c2, .c3, .c4, .c5, .c6, .c7, .c8] {
			eprintln('>> x: $x | hex value: ${u64(x).hex()}')
		}
		eprintln('----------------------------------------------')
		for x in [PawnsBoard.d1, .d2, .d3, .d4, .d5, .d6, .d7, .d8] {
			eprintln('>> x: $x | hex value: ${u64(x).hex()}')
		}
		eprintln('----------------------------------------------')
		for x in [PawnsBoard.e1, .e2, .e3, .e4, .e5, .e6, .e7, .e8] {
			eprintln('>> x: $x | hex value: ${u64(x).hex()}')
		}
		eprintln('----------------------------------------------')
		for x in [PawnsBoard.f1, .f2, .f3, .f4, .f5, .f6, .f7, .f8] {
			eprintln('>> x: $x | hex value: ${u64(x).hex()}')
		}
		eprintln('----------------------------------------------')
		for x in [PawnsBoard.g1, .g2, .g3, .g4, .g5, .g6, .g7, .g8] {
			eprintln('>> x: $x | hex value: ${u64(x).hex()}')
		}
		eprintln('----------------------------------------------')
		for x in [PawnsBoard.h1, .h2, .h3, .h4, .h5, .h6, .h7, .h8] {
			eprintln('>> x: $x | hex value: ${u64(x).hex()}')
		}
	}
}
