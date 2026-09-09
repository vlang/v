module mymod

pub interface Context {}

pub type HandlerFn = fn (c Context) Error

pub type MiddlewareFn = fn (next HandlerFn) HandlerFn

pub struct App {
mut:
	middlewares []MiddlewareFn
}

// use appends middleware handlers in registration order.
pub fn (mut app App) use(middleware ...MiddlewareFn) {
	app.middlewares << middleware
}

// new returns an empty application.
pub fn new() App {
	return App{}
}
