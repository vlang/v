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

fn test_helper_scope_retention_checks_the_combined_budget() {
	assert !transform_helper_scopes_fit_retention_budget([]voidptr{})
	assert !transform_helper_scopes_fit_retention_budget([voidptr(unsafe { nil })])
	$if prealloc {
		first := prealloc_scope_begin_for_v3()
		text := 'borrowed helper text'.clone()
		prealloc_scope_leave_for_v3(first)
		second := prealloc_scope_begin_for_v3()
		prealloc_scope_leave_for_v3(second)
		defer {
			prealloc_scope_free_for_v3(second)
			prealloc_scope_free_for_v3(first)
		}
		assert transform_helper_scopes_fit_retention_budget([first, second])
		assert text == 'borrowed helper text'
		// Repeating a real arena exercises accumulation without a large allocation.
		size := unsafe { prealloc_scope_allocated_size(first) }
		count := int(usize(256 * 1024 * 1024) / size)
		assert transform_helper_scopes_fit_retention_budget([]voidptr{len: count, init: first})
		assert !transform_helper_scopes_fit_retention_budget([]voidptr{len: count + 1, init: first})
	}
}
