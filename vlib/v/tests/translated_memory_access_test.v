@[has_globals; translated]
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

__global translated_count = int(5)

fn translated_count_argument(translated_count int) int {
	return translated_count
}

fn translated_state_alias(state &TranslatedState) &TranslatedState {
	return state
}

fn test_translated_call_result_field_writes() {
	state := TranslatedState{ count: 40 }
	translated_state_alias(&state).count |= 2
	assert state.count == 42
	translated_state_alias(&state).count = 43
	assert state.count == 43
}

fn test_translated_global_name_shadowing() {
	assert translated_count_argument(42) == 42
	assert translated_count == 5
	if true {
		translated_count := 43
		assert translated_count == 43
	}
	assert translated_count == 5
}
