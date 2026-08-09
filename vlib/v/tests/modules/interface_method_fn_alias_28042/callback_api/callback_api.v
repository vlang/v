module callback_api

pub interface EventData {}

pub type Handler = fn (EventData)

pub interface Initializer {
mut:
	connect(string, string, Handler)
}

// initialize registers the callback through the supplied initializer.
pub fn initialize(mut initializer Initializer) {
	initializer.connect('branch', 'event', fn (_ EventData) {})
}
