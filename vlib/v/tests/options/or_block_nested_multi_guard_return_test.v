struct GuardFallbackBox {
	value int
}

fn guard_fallback_box(fail bool) !GuardFallbackBox {
	if fail {
		return error('box failure')
	}
	return GuardFallbackBox{ value: 42 }
}

fn guard_fallback_pair(present bool) ?(int, int) {
	if present {
		return 2, 3
	}
	return none
}

fn nested_multi_guard_then_return(fail bool, present bool) !GuardFallbackBox {
	box := guard_fallback_box(fail) or {
		n := if _, _ := guard_fallback_pair(present) { return err } else { 1 }
		GuardFallbackBox{ value: n }
	}
	return box
}

fn nested_multi_guard_else_return(fail bool, present bool) !GuardFallbackBox {
	box := guard_fallback_box(fail) or {
		n := if a, b := guard_fallback_pair(present) { a + b } else { return err }
		GuardFallbackBox{ value: n }
	}
	return box
}

fn nested_multi_guard_struct_value(present bool) !GuardFallbackBox {
	box := guard_fallback_box(true) or {
		other := if a, b := guard_fallback_pair(present) {
			GuardFallbackBox{ value: a + b }
		} else {
			return err
		}
		other
	}
	return box
}

fn test_result_or_block_nested_multi_guard_uses_its_integer_type() {
	first := nested_multi_guard_then_return(false, true) or { panic(err) }
	assert first.value == 42
	fallback := nested_multi_guard_then_return(true, false) or { panic(err) }
	assert fallback.value == 1
	if _ := nested_multi_guard_then_return(true, true) {
		assert false
	} else {
		assert err.msg() == 'box failure'
	}

	second := nested_multi_guard_else_return(false, false) or { panic(err) }
	assert second.value == 42
	bound := nested_multi_guard_else_return(true, true) or { panic(err) }
	assert bound.value == 5
	if _ := nested_multi_guard_else_return(true, false) {
		assert false
	} else {
		assert err.msg() == 'box failure'
	}
}

fn test_result_or_block_nested_multi_guard_retains_struct_values() {
	value := nested_multi_guard_struct_value(true) or { panic(err) }
	assert value.value == 5
	if _ := nested_multi_guard_struct_value(false) {
		assert false
	} else {
		assert err.msg() == 'box failure'
	}
}
