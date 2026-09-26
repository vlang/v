module driver

// In a parallel build, parallel transform follows the default and `-no-parallel`.
fn test_parallel_transform_follows_no_parallel_in_parallel_builds() {
	assert v3_parallel_transform_allowed(true, false)
	assert !v3_parallel_transform_allowed(true, true)
	assert !v3_parallel_transform_allowed(false, false)
}
