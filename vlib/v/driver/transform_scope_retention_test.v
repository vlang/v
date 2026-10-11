module driver

fn test_transform_scope_retention_budget_bounds_allocation_size() {
	limit := usize(64 * 1024 * 1024)
	assert !transform_scope_size_is_bounded(0)
	assert transform_scope_size_is_bounded(1)
	assert transform_scope_size_is_bounded(limit)
	assert !transform_scope_size_is_bounded(limit + 1)
	assert !transform_scope_size_is_bounded(~usize(0))
}

fn test_transform_scope_retention_requires_an_actual_arena() {
	assert !transform_scope_fits_retention_budget(unsafe { nil })
}
