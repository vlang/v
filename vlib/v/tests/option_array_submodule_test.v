import another_module as amodule

fn test_submodule_array_instance() {
	x := ?[]amodule.SomeStruct{}
	dump(x)
	assert x == none

	y := ?amodule.SomeStruct(none)
	dump(y)
	assert y == none

	w := ?[]amodule.SomeStruct(none)
	dump(w)
	assert w == none

	z := ?[2]amodule.SomeStruct(none)
	dump(z)
	assert z == none
}
