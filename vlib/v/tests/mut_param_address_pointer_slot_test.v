struct PointerSlotContext {
mut:
	value int
}

fn increment_pointer_slot(mut ctx &PointerSlotContext) {
	ctx.value++
}

fn forward_address_of_mut_value(mut ctx PointerSlotContext) {
	increment_pointer_slot(mut &ctx)
}

fn forward_address_of_mut_value_generic[T](mut ctx T) {
	increment_pointer_slot(mut &ctx)
}

fn test_address_of_mut_value_parameter_to_pointer_slot() {
	mut ctx := PointerSlotContext{}
	increment_pointer_slot(mut &ctx)
	assert ctx.value == 1
	forward_address_of_mut_value(mut ctx)
	assert ctx.value == 2
	forward_address_of_mut_value_generic(mut ctx)
	assert ctx.value == 3
}
