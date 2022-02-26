module some_module

import eventbus

const (
	eb = eventbus.new()
)

pub struct Duration {
pub:
	hours int
}

pub struct EventMetadata {
pub:
	message string
}

pub fn do_work() {
	duration := Duration{10}
	for i in 0 .. 10 {
		println('working...')
		if i == 5 {
			event_metadata := &EventMetadata{'Iteration ' + i.str()}
			some_module.eb.publish('event_foo', duration, event_metadata)
			some_module.eb.publish('event_bar', duration, event_metadata)
		}
	}
	some_module.eb.publish('event_baz', &Duration{42}, &EventMetadata{'Additional data at the end.'})
}

pub fn get_subscriber() eventbus.Subscriber {
	return *some_module.eb.subscriber
}
