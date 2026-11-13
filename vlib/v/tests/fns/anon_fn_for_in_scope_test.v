fn callback_with_independent_loop_bindings(item int) fn () int {
	assert item == 99
	return fn () int {
		mut values := [1, 2, 3]
		for mut item in values {
			item *= 2
		}
		mut total := 0
		for index, item in values {
			nested := fn () int {
				mut sum := 0
				for item in [4, 5] {
					sum += item
				}
				return sum
			}
			total += index + item + nested()
		}
		return total
	}
}

fn test_anonymous_functions_have_independent_loop_bindings() {
	callback := callback_with_independent_loop_bindings(99)
	assert callback() == 42
}

fn test_anonymous_functions_keep_explicit_captures_and_restore_outer_bindings() {
	item := 10
	offset := 3
	callback := fn [offset] () int {
		mut sum := offset
		for item in [1, 2] {
			sum += item
		}
		return sum
	}
	outer := fn [item] () int {
		return item
	}
	assert callback() == 6
	assert outer() == 10
	assert item == 10
}
