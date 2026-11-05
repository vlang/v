@[heap]
struct PointerParamCaptureState {
mut:
	value int
}

fn pointer_param_capture_identity(state &PointerParamCaptureState) &PointerParamCaptureState {
	return state
}

fn make_mut_pointer_param_capture(mut state &PointerParamCaptureState) fn (&PointerParamCaptureState) {
	return fn [mut state] (replacement &PointerParamCaptureState) {
		alias := state
		assert typeof(state).name == '&PointerParamCaptureState'
		assert typeof(alias).name == '&PointerParamCaptureState'
		assert voidptr(alias) == voidptr(state)
		assert voidptr(pointer_param_capture_identity(state)) == voidptr(state)
		state.value++
		state = replacement
		state.value += 10
	}
}

fn test_mut_pointer_param_capture_keeps_the_callers_pointer_slot() {
	mut state := &PointerParamCaptureState{ value: 1 }
	first := state
	callback := make_mut_pointer_param_capture(mut state)
	second := &PointerParamCaptureState{ value: 100 }
	callback(second)
	assert first.value == 2
	assert voidptr(state) == voidptr(second)
	assert state.value == 110

	state = &PointerParamCaptureState{ value: 200 }
	third := state
	replacement := &PointerParamCaptureState{ value: 300 }
	callback(replacement)
	assert third.value == 201
	assert voidptr(state) == voidptr(replacement)
	assert state.value == 310
	assert second.value == 110
}

fn make_pointer_param_snapshot(mut state &PointerParamCaptureState) fn () &PointerParamCaptureState {
	return fn [state] () &PointerParamCaptureState {
		return pointer_param_capture_identity(state)
	}
}

fn test_immutable_pointer_param_capture_keeps_its_pointer_snapshot() {
	mut state := &PointerParamCaptureState{ value: 7 }
	first := state
	callback := make_pointer_param_snapshot(mut state)
	state = &PointerParamCaptureState{ value: 9 }
	assert voidptr(callback()) == voidptr(first)
	assert callback().value == 7
	assert state.value == 9
}

fn make_nested_mut_pointer_param_capture(mut state &PointerParamCaptureState) fn (&PointerParamCaptureState) {
	return fn [mut state] (replacement &PointerParamCaptureState) {
		fn [mut state, replacement] () {
			state = replacement
			state.value += 5
		}()
	}
}

fn test_nested_mut_pointer_param_capture_keeps_the_callers_pointer_slot() {
	mut state := &PointerParamCaptureState{ value: 1 }
	first := state
	callback := make_nested_mut_pointer_param_capture(mut state)
	replacement := &PointerParamCaptureState{ value: 20 }
	callback(replacement)
	assert first.value == 1
	assert voidptr(state) == voidptr(replacement)
	assert state.value == 25
}

fn make_mut_double_pointer_param_capture(mut state &&PointerParamCaptureState) fn (&&PointerParamCaptureState) {
	return fn [mut state] (replacement &&PointerParamCaptureState) {
		alias := state
		assert typeof(state).name == '&&PointerParamCaptureState'
		assert typeof(alias).name == '&&PointerParamCaptureState'
		assert voidptr(alias) == voidptr(state)
		state = replacement
		assert voidptr(state) == voidptr(replacement)
	}
}

fn test_mut_pointer_param_capture_preserves_explicit_pointer_depth() {
	mut first := &PointerParamCaptureState{ value: 1 }
	mut second := &PointerParamCaptureState{ value: 2 }
	mut state := &first
	callback := make_mut_double_pointer_param_capture(mut state)
	callback(&second)
	assert voidptr(state) == voidptr(&second)
	assert first.value == 1
	assert second.value == 2
}
