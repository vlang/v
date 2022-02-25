module some_module

import eventbus

const (
	eb = eventbus.new()
)

pub struct Work {
pub:
	hours int
}

pub struct Event {
pub:
	message string
}

pub fn do_work() {
	work := Work{10}
	for i in 0 .. 10 {
		println('working...')
		if i == 5 {
			error := &Event{'Iteration '+i.str()}
			some_module.eb.publish('event_foo', work, error)
			some_module.eb.publish('event_bar', work, error)
		}
	}
	some_module.eb.publish('event_bar', &Work{42}, &Event{'Additional data at the end.'})
}

pub fn get_subscriber() eventbus.Subscriber {
	return *some_module.eb.subscriber
}
