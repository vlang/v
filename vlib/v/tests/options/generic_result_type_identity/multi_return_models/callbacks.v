module multi_return_models

pub struct CallbackPoint {
pub mut:
	x int
}

pub struct Callbacks[T] {
pub:
	callback ?fn (mut ?CallbackPoint, T)
}

// apply unwraps and calls a generic optional callback with a mutable option parameter.
pub fn (callbacks Callbacks[T]) apply(mut point ?CallbackPoint, config T) {
	callback := callbacks.callback or { return }
	callback(mut point, config)
}
