module callback_api

pub interface EventData {}

pub type Handler = fn (EventData)

pub interface Initializer {
mut:
	connect(string, string, Handler)
}

pub fn initialize(mut initializer Initializer) {
	initializer.connect('branch', 'event', fn (_ EventData) {})
}
