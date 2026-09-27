module main

import interface_from_another_module.mod

interface IBar {
	mod.IFoo
}

struct Abc {}

fn test_interface() {
	a := IBar(Abc{})
	dump(a)
	assert true
}
