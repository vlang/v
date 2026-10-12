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

fn forward_caller_context[A, C](app A, mut ctx C) {
	dispatch.outer[A, C](app, mut ctx)
}

fn test_imported_generic_pointer_storage_from_a_mut_parameter() {
	mut ctx := Context{ value: 5 }
	forward_caller_context(App{}, mut ctx)
	assert ctx.value == 6
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
