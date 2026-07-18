// Regression test for: `for x in low .. high` where `low` has a narrow
// integer type (e.g. `u8`) and `high` is a compile-time constant that
// overflows that type (e.g. `min_u8 .. int(max_u8) + 1`) is now a compile
// error instead of silently generating an infinite loop.

fn test_full_u8_range_works_when_low_is_widened_to_int() {
	// Using `int(min_u8)` (or a plain `0`) keeps the loop variable's type as
	// `int`, which can represent the full 0..256 range without overflowing.
	mut count := 0
	for b in int(min_u8) .. int(max_u8) + 1 {
		count++
		_ := b
	}
	assert count == 256
}

fn test_plain_zero_low_also_works_for_full_u8_range() {
	mut count := 0
	for b in 0 .. int(max_u8) + 1 {
		count++
		_ := b
	}
	assert count == 256
}

fn test_partial_u8_range_still_works() {
	mut count := 0
	for b in min_u8 .. u8(200) {
		count++
		_ := b
	}
	assert count == 200
}
