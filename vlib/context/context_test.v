// vtest build: amd64 || arm64
module context

import time

// UserContext stands in for a `Context` implemented outside this module, which is
// the only case that reaches the `else` branch of `Context.str`.
struct UserContext {
mut:
	ch chan int
}

fn (ctx UserContext) deadline() ?time.Time {
	return none
}

fn (ctx UserContext) value(_ Key) ?Any {
	return none
}

fn (mut ctx UserContext) done() chan int {
	return ctx.ch
}

fn (mut ctx UserContext) err() IError {
	return none
}

fn test_str_of_a_user_defined_context_names_its_concrete_type() {
	ctx := Context(UserContext{})
	assert ctx.str() == 'UserContext'
}
