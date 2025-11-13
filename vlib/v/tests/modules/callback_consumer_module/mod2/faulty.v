module mod2

pub fn function() {
	callback_consumer(callback_fn)
}

type CallbackType = fn () [4]u8

fn callback_consumer(callback CallbackType) {
	callback()
}

fn callback_fn() [4]u8 {
	return [u8(1), 2, 3, 255]!
}
