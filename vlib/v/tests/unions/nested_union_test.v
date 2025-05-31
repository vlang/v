union MyNested1 {
mut:
	a            i32
	b            i32
	nested_union union {
	mut:
		c f32
		d char
	}
}

fn test_nested_unions() {
	mut m := MyNested1{}
	unsafe {
		m.a = 12
		assert m.b == 12
		println(m.a)
		m.b = -99
		assert m.b == -99
		println(m.b)
		m.nested_union.c = 3.14
		assert m.nested_union.c == 3.14
		println(m.nested_union.c)
		m.nested_union.d = 88
		println(int(m.nested_union.d))
		assert int(m.nested_union.d) == 88
	}
}
