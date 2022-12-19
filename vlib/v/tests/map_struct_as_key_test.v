struct Abc {
	x int
	y int
}

fn (s &Abc) map_hash() u64 {
	//    eprintln('${@METHOD} called, s: $s')
	return u64(s.x) << 32 + u64(s.y)
}

fn (s &Abc) map_eq(b &Abc) bool {
	//    eprintln('${@METHOD} called, s: $s')
	return s.x == b.x && s.y == b.y
}

fn (mut s Abc) map_clone(new &Abc) {
	//    eprintln('${@METHOD} called, s: $s | new: $new')
	unsafe {
		*s = *new
	}
}

fn (s &Abc) map_free() {
	eprintln('${@METHOD} called, s: ${s}')
}

fn test_map_with_struct_as_key() {
	s0 := Abc{}
	s1 := Abc{1, 2}
	mut m := map[Abc]int{}
	m[s0] = 123
	m[s1] = 456
	assert m[s0] == 123
	assert m[s1] == 456
	dump(m)
	m[s1] = 789
	dump(m[s1])
	dump(m)
	assert m[s0] == 123
	assert m[s1] == 789
}
