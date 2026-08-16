struct MutPointerFreeState {
mut:
	values []int
}

fn reset_mut_pointer_free_state(mut state &MutPointerFreeState) {
	// Free the array's storage, then immediately reset the field to an empty array so a
	// later ownership drop cannot free the same allocation twice.
	unsafe {
		state.values.free()
	}
	state.values = []
}

fn test_mut_pointer_param_array_field_free_codegen() {
	mut state := &MutPointerFreeState{
		values: [1, 2, 3]
	}
	reset_mut_pointer_free_state(mut state)
	assert state.values.len == 0
}
