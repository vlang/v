module main

import mod

interface IBar {
	mod.IFo
}

struct Abc {}

fn test_interface() {
	a := IBar(Abc{})
	dump(a)
	assert true
}
