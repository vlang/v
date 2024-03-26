struct St {
mut:
	a []u8
}

pub fn (s St) str() string {
	return s.a.bytestr()
}

fn confuser(mut s St) {
	assert s.str() == 'a'
}

fn test_str_method_with_mut() {
	mut s := St{
		a: []u8{}
	}
	s.a << `a`
	confuser(mut s)
	assert s.str() == 'a'
}
