type StagedValue = int | string

type StagedMap = map[string]StagedValue

fn select_staged[T](take bool, first T, second T) T {
	return if take { first } else { second }
}

fn match_staged[T](take bool, first T, second T) T {
	return match take {
		true { first }
		false { second }
	}
}

fn recover_staged[T](value ?T, fallback T) T {
	return value or { fallback }
}

fn staged_increment(value int) int {
	return value + 1
}

fn staged_decrement(value int) int {
	return value - 1
}

fn test_staging_preserves_selected_values_and_reference_identity() {
	first := StagedMap({
		'key': StagedValue(42)
	})
	second := StagedMap({
		'key': StagedValue('second')
	})
	assert select_staged(true, first, second) == first
	assert match_staged(false, first, second) == second
	missing := ?StagedMap(none)
	assert recover_staged(missing, first) == first
	assert recover_staged(?StagedMap(second), first) == second
	first_array := [first, second]!
	second_array := [second, first]!
	assert select_staged(false, first_array, second_array)[0] == second
	assert match_staged(true, first_array, second_array)[0] == first
	first_dynamic := [StagedValue(42)]
	second_dynamic := [StagedValue('second')]
	selected := select_staged(false, first_dynamic, second_dynamic)
	assert selected == second_dynamic
	assert selected.data == second_dynamic.data
	callback := select_staged(true, staged_increment, staged_decrement)
	assert callback(41) == 42
	value := StagedValue(42)
	other := StagedValue('other')
	pointer := select_staged(false, &value, &other)
	assert voidptr(pointer) == voidptr(&other)
	assert *pointer == other
}

fn staged_optional(take bool) ?int {
	return match take {
		true { 42 }
		false { none }
	}
}

fn staged_result(take bool) !int {
	return match take {
		true { 42 }
		false { error('missing') }
	}
}

fn test_staging_preserves_wrapped_match_contexts() {
	assert staged_optional(true)? == 42
	assert staged_optional(false) == none
	assert staged_result(true)! == 42
	staged_result(false) or {
		assert err.msg() == 'missing'
		return
	}
	assert false
}
