struct Serials {
	a u8
	b u8
	c u8
pub mut:
	ids map[u8][3]u8
}

pub fn (mut s Serials) id(id u8) {
	if _ := s.ids[id] {
	} else {
		s3 := u8(s.c + s.ids.len)
		s.ids[id] = [s.a, s.a, s3]!
	}
}

fn test_map_fixed_array_if_guard() {
	mut s := Serials{
		a: 1
		b: 2
	}
	s.id(u8(3))
	println(s)
	assert s.ids[3] == [u8(1), 1, 0]!

	s.id(u8(4))
	println(s)
	assert s.ids[4] == [u8(1), 1, 1]!
}
