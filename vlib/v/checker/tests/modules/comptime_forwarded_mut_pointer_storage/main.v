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

fn main() {
	mut ctx := Context{}
	dispatch.outer(App{}, mut ctx)
}
