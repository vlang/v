module main

import v.tests.generics.generic_default_modules.nesteddefaults

struct Local {
	x int = 11
}

struct LocalPointerBox[T] {
	value &T = &T{}
}

fn test_imported_nested_generic_struct_defaults_use_local_type() {
	box := nesteddefaults.Box[Local]{}
	assert box.value.x == 11
	wrapper := nesteddefaults.Wrapper[Local]{}
	assert wrapper.box.value.x == 11
	pointer := nesteddefaults.PointerBox[Local]{}
	assert pointer.value.x == 11
	local_pointer := LocalPointerBox[Local]{}
	assert local_pointer.value.x == 11
}
