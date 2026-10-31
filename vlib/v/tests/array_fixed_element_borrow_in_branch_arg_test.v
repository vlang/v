fn sum_borrowed_f32(values &f32, count int) f32 {
	if values == unsafe { nil } {
		return -1
	}
	mut total := f32(0)
	for i in 0 .. count {
		total += unsafe { values[i] }
	}
	return total
}

fn sum_selected(use_values bool) f32 {
	radii := [f32(1), 2, 3, 4]!
	return sum_borrowed_f32(if use_values { &radii[0] } else { unsafe { &f32(nil) } }, radii.len)
}

fn sum_matched(kind int) f32 {
	radii := [f32(5), 6]!
	return sum_borrowed_f32(match kind {
		0 { &radii[0] }
		else { &radii[1] }
	}, 1)
}

fn test_fixed_array_element_borrowed_through_a_branch_value_argument() {
	assert sum_selected(true) == 10
	assert sum_selected(false) == -1
	assert sum_matched(0) == 5
	assert sum_matched(1) == 6
}
