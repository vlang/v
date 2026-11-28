import math

fn oversized_shift_count() u64 {
	return u64(64)
}

// math has a private `const shift = 52`. The local `shift` below is not that
// constant, so its shifts keep their guard against counts of 64 and more.
fn test_local_shift_count_named_like_a_module_const_keeps_its_guard() {
	shift := oversized_shift_count()
	assert u64(9221120237041090561) >> shift == u64(0)
	assert u64(1) << shift == u64(0)
	assert math.abs(-1) == 1
}
