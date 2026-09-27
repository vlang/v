module foreign

pub struct Event {
pub:
	value int
}

// event_value keeps the foreign Event type reachable.
pub fn event_value() int { return Event{ value: 9 }.value }
