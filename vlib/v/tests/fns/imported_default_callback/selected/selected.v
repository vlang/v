module selected

import foreign { Event }

pub struct ImportedDefaults {
pub:
	on_event   fn (&Event) int = fn (event &Event) int {
		return event.value
	}
	make_event fn () Event = fn () Event {
		return Event{ value: 73 }
	}
}
