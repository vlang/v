fn make_mut_array_appender(mut values []int) fn (int) {
	return fn [mut values] (value int) {
		values << value
	}
}

fn make_array_parameter_snapshot(mut values []int) fn () int {
	return fn [values] () int {
		return values.len
	}
}

fn test_mut_array_parameter_capture_updates_callers_array_header() {
	mut values := []int{}
	first := make_mut_array_appender(mut values)
	second := make_mut_array_appender(mut values)
	first(7)
	assert values == [7]
	second(9)
	first(11)
	assert values == [7, 9, 11]
}

fn test_immutable_array_parameter_capture_keeps_its_snapshot() {
	mut values := [7]
	snapshot := make_array_parameter_snapshot(mut values)
	values << 9
	assert snapshot() == 1
	assert values == [7, 9]
}
