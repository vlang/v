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

interface Any {}

struct Thing {}

union Bad {
	f f64
	a Any
}

union BadBits {
	f   f64
	bad Bad
}

fn bad_from_float(x f64) Bad {
	bits := BadBits{
		f: x
	}
	return unsafe { bits.bad }
}

fn test_union_str_with_invalid_interface_member_does_not_crash() {
	x := [bad_from_float(2.0)]
	assert x.str() == '[Bad{\n    f: 2.0\n    a: unknown interface value\n}]'
}

fn test_union_str_with_valid_interface_member_still_works() {
	x := Bad{
		a: Thing{}
	}
	assert x.str().contains('a: Any(Thing{})')
}
