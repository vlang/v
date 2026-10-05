module main

import bridge as dispatch

pub struct Context {
pub mut:
	value int
}

pub struct App {}

// index increments the pointed-to context.
pub fn (_ App) index(mut ctx &Context) {
	ctx.value++
}

fn test_imported_generic_forwarded_pointer_storage() {
	mut ctx := Context{}
	dispatch.outer(App{}, mut ctx)
	assert ctx.value == 1
}

fn test_imported_forwarding_keeps_same_named_types_distinct() {
	mut helper_context := dispatch.Context{}
	dispatch.outer(dispatch.App{}, mut helper_context)
	assert helper_context.value == 10
	mut caller_context := Context{}
	dispatch.outer(App{}, mut caller_context)
	assert caller_context.value == 1
	dispatch.outer(dispatch.App{}, mut helper_context)
	assert helper_context.value == 20
}
