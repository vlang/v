module main

import some_module as sm

@[markused]
type PFN_set_enum_array_submodule = fn (voidptr, [2]sm.Flag_bits2)

fn test_fn_type_with_submodule_enum_fixed_array_second_param() {
	assert true
}
