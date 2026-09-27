@[translated]
module main

struct BorrowState {
	value int
}

struct BorrowHandle {
	pointer &BorrowState
}

fn borrowed_state(state &BorrowState) int {
	alias := state
	handle := BorrowHandle{ pointer: state }
	return alias.value + handle.pointer.value
}

fn offset_first(value &int) int {
	return unsafe { *value }
}

fn test_translated_pointer_aliases() {
	state := BorrowState{21}
	assert borrowed_state(&state) == 42
}

fn test_translated_fixed_array_offset_borrow() {
	values := [3, 5]!
	assert offset_first(&values[0] + 1) == 5
	assert offset_first(&values[1] - 1) == 3
	assert offset_first((&values[0]) + 1) == 5
}

fn test_translated_indexed_pointer_cast_address() {
	state := BorrowState{42}
	pointer := &(&BorrowState(voidptr(&state)))[0]
	assert pointer.value == 42
	text := &c'hello'[1]
	assert unsafe { text.vstring() } == 'ello'
}
