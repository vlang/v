// A negated or folded integer constant in an `or {}` block of a float option
// is untyped, like a bare integer literal.
const fallback_width = 4

struct SnapshotState {
mut:
	width          ?f64
	snapshot_width f64
}

fn maybe_f32(ok bool) ?f32 {
	if ok {
		return 1.5
	}
	return none
}

fn test_negative_int_literal_in_float_or_block() {
	mut state := SnapshotState{
		snapshot_width: 5.0
	}
	assert state.width or { -1 } != state.snapshot_width
	assert state.width or { -1 } == -1.0
	state.width = 5.0
	assert !(state.width or { -1 } != state.snapshot_width)
	state.width = none
	assert state.width or {
		-1
	} == -1.0
	value := state.width or { -1 }
	assert value == -1.0
	assert typeof(value).name == 'f64'
}

fn test_folded_int_constant_in_float_or_block() {
	state := SnapshotState{}
	assert state.width or { -(2) } == -2.0
	assert state.width or { 1 + 2 } == 3.0
	assert state.width or { fallback_width } == 4.0
	assert state.width or { -fallback_width } == -4.0
	assert maybe_f32(false) or { -1 } == f32(-1.0)
	assert maybe_f32(true) or { -1 } == f32(1.5)
}
