#flag -I @VEXEROOT/vlib/v/tests/c_struct_free
#include "free_struct.c"

struct C.foo {
mut:
	free int
}

fn test_free_property_on_c_struct() {
	mut a := C.foo{0}
	a.free = 2
	assert a.free == 2
}
