module api

import foreign

pub struct Event {
pub:
	value int
}

pub struct Window {
pub mut:
	count int
pub:
	on_event fn (e &Event, mut w Window) = fn (_ &Event, mut _ Window) {}
}

pub struct WindowCfg {
pub:
	on_init  fn (mut Window)             = fn (mut _ Window) {}
	on_event fn (e &Event, mut w Window) = fn (_ &Event, mut _ Window) {}
}

pub struct ResultDefaults {
pub:
	make_event fn () Event = fn () Event {
		return Event{ value: 42 }
	}
	make_pair  fn () (Event, int) = fn () (Event, int) {
		return Event{ value: 43 }, 1
	}
	on_channel fn (chan Event) = fn (events chan Event) {}
}

// window initializes a window and preserves its default callback.
pub fn window(cfg WindowCfg) Window {
	mut result := Window{ count: foreign.event_value(), on_event: cfg.on_event }
	cfg.on_init(mut result)
	return result
}
