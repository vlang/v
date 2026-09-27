module main

import bridge

fn test_methods_on_c_struct_fields_and_local_copies() {
	h := bridge.make_holder()
	assert h.value.read() == 17
	assert h.value.read_again() == 17
}
