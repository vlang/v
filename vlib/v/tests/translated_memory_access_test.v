@[translated]
module main

struct TranslatedState {
	count int
}

fn increment_translated_state(state &TranslatedState) {
	state.count++
	state.count += 2
}

fn test_translated_struct_field_writes() {
	state := &TranslatedState{}
	increment_translated_state(state)
	assert state.count == 3
}

fn test_translated_negative_pointer_index() {
	mut values := [3, 5]!
	pointer := unsafe { &values[1] }
	assert pointer[-1] == 3
	pointer[-1] = 7
	assert values[0] == 7
}
