// vfmt off
@[flag]
enum Bits8 as u8 {
	a1 b1 c1 d1 e1 f1 g1 h1
}
@[flag]
enum Bits16 as u16 {
	a1 b1 c1 d1 e1 f1 g1 h1
	a2 b2 c2 d2 e2 f2 g2 h2
}
@[flag]
enum Bits32 as u32 {
	a1 b1 c1 d1 e1 f1 g1 h1
	a2 b2 c2 d2 e2 f2 g2 h2
	a3 b3 c3 d3 e3 f3 g3 h3
	a4 b4 c4 d4 e4 f4 g4 h4
}
@[flag]
enum Bits64 as u64 {
	a1 b1 c1 d1 e1 f1 g1 h1
	a2 b2 c2 d2 e2 f2 g2 h2
	a3 b3 c3 d3 e3 f3 g3 h3
	a4 b4 c4 d4 e4 f4 g4 h4
	a5 b5 c5 d5 e5 f5 g5 h5
	a6 b6 c6 d6 e6 f6 g6 h6
	a7 b7 c7 d7 e7 f7 g7 h7
	a8 b8 c8 d8 e8 f8 g8 h8
}
// vfmt on

fn check_map[T](size int) {
	println('>>> checking map of ${T.name:10} enum keys, size should be: ${size}')
	mut m := map[T]u32{}
	for i in 0 .. size {
		n := u64(1) << i
		m[unsafe { T(n) }] = i
		// eprintln('>>> i: ${i:2} | n: ${n:20} | m.len: ${m.len}')
	}
	assert m.len == size
}

fn test_maps_with_enum_keys_work() {
	check_map[Bits8](8)
	check_map[Bits16](16)
	check_map[Bits32](32)
	check_map[Bits64](64)
}
