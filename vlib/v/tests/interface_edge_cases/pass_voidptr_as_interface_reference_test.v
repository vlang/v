interface IAbc {
	xyz()
}

struct Abc {}

fn (a Abc) xyz() {}

fn f(i &IAbc) string {
	return '${i}'
}

fn test_passing_voidptr_as_an_interface_reference() {
	i := IAbc(Abc{})
	assert f(&i) == '&IAbc(Abc{})'
	// a voidptr() cast is an escape hatch, that should be allowed
	// but perhaps it should be forced by the compiler to be in unsafe{}
	assert f(unsafe { voidptr(u64(0)) }) == '&IAbc(0x0)'
}
