// Regression tests
// `for x in low .. high` where `low` has a narrow integer type (e.g. `u8`)
// and `high` is a compile-time constant that overflows that type (e.g. `min_u8 .. int(max_u8) + 1`)
// is now a compile error instead of silently generating an infinite loop.

fn test_full_u8_range_works_when_low_is_widened_to_int() {
	// Using `int(min_u8)` (or a plain `0`) keeps the loop variable's type as
	// `int`, which can represent the full 0..256 range without overflowing.
	mut count := 0
	for _ in int(min_u8) .. int(max_u8) + 1 {
		count++
	}
	assert count == 256
}

fn test_plain_zero_low_also_works_for_full_u8_range() {
	mut count := 0
	for _ in 0 .. int(max_u8) + 1 {
		count++
	}
	assert count == 256
}

fn test_partial_u8_range_still_works() {
	mut count := 0
	for _ in min_u8 .. u8(200) {
		count++
	}
	assert count == 200
}

fn test_dynamic_bounds_range_is_unaffected() {
	low := 0
	high := 5
	mut count := 0
	for _ in low .. high {
		count++
	}
	assert count == 5
}

fn test_explicit_wider_high_type_is_not_flagged() {
	// Check that the bound is accepted, without relying on the loop variable
	// being widened. Stop before incrementing past the largest u8 value.
	mut count := 0
	for b in u8(0) .. u16(256) {
		assert int(b) == count
		count++
		if count == 256 {
			break
		}
	}
	assert count == 256
}

fn test_typed_low_with_concrete_int_high_keeps_low_type() {
	arr := [10, 20, 30]
	mut count := 0
	for _ in u8(0) .. arr.len {
		count++
	}
	assert count == 3
}

fn test_dynamic_high_outside_low_type_is_not_flagged() {
	// Keep the high bound dynamic and outside u8, but do not overflow int
	// while computing it, or let the narrow loop variable wrap around.
	n := int(max_u8)
	mut count := 0
	for b in u8(0) .. n + 1 {
		assert int(b) == count
		count++
		if count == 256 {
			break
		}
	}
	assert count == 256
}

fn test_dynamic_empty_range_is_unaffected() {
	n := -1
	mut count := 0
	for _ in u8(0) .. n + 1 {
		count++
		break
	}
	assert count == 0
}
