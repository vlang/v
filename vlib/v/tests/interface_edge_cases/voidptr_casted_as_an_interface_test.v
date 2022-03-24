interface IAbc {
	name string
	xyz()
}

struct Abc {
	name string
	x    int = 123
}

fn (a Abc) xyz() {}

fn f(i &IAbc) string {
	return '$i'
}

fn test_voidptr_casted_as_an_interface_reference() {
	mut pi := &IAbc(voidptr(0))
	dump(pi)
	assert f(pi) == '&IAbc(0x0)'
	//
	mut i := IAbc(Abc{})
	pi = &i
	dump(pi)
	assert f(pi).contains('x: 123')
}
