interface Speaker {
	speak() string
}

union MyUnion implements Speaker {
	vint    int
	vu32    u32
	vstring string
}

fn (u MyUnion) speak() string {
	return 'hi, u.vint: ${unsafe { u.vint }}'
}

fn test_union_implementing_interface() {
	s := Speaker(MyUnion{
		vint: 123
	})
	assert s.speak() == 'hi, u.vint: 123'
}
