struct PointerCastContext {
mut:
	value int
}

fn update_pointer_cast_context(mut context &PointerCastContext) {
	context.value++
}

fn test_mut_pointer_cast_context() {
	mut context := &PointerCastContext{ value: 41 }
	ctx := voidptr(context)
	update_pointer_cast_context(mut unsafe { &PointerCastContext(ctx) })
	assert context.value == 42
}
