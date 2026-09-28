module main

import bridge

fn test_methods_on_c_struct_fields_and_local_copies() {
	h := bridge.make_holder()
	assert h.value.read() == 17
	assert h.value.convert[int](1) == 17
	assert h.value.read_again() == 17
	alias_value := bridge.Counter(h.value)
	assert alias_value.read() == 117
	assert alias_value.convert[int](1) == 217
	assert alias_value.alias_only[int](1) == 317
}
