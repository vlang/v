fn test_closure_local_can_shadow_outer_local() {
	mut values := []string{}
	apply_with_context(&values, fn (ctx voidptr) {
		mut values := unsafe { &[]string(ctx) }
		values << 'ok'
	})
	assert values == ['ok']
}

fn apply_with_context(ctx voidptr, callback fn (voidptr)) {
	callback(ctx)
}
