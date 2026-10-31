interface NamedVar {
	name &u8
	id() int
}

struct NamedFlag {
	name &u8 = unsafe { nil }
	n    int
}

fn (f &NamedFlag) id() int {
	return f.n
}

fn (v NamedVar) has_name() bool {
	return v.name != unsafe { nil }
}

fn named_var_first_byte(v NamedVar) u8 {
	if v.name == unsafe { nil } {
		return 0
	}
	return unsafe { *v.name }
}

fn test_interface_pointer_field_named_name() {
	a := NamedVar(&NamedFlag{c'on', 1})
	b := NamedVar(&NamedFlag{
		n: 2
	})
	assert a.has_name()
	assert !b.has_name()
	assert named_var_first_byte(a) == `o`
	assert named_var_first_byte(b) == 0
	assert a.id() + b.id() == 3
}
