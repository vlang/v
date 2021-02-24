module some_module

import eventbus

const (
	eb = eventbus.new()
)

pub struct Work {
pub:
	hours int
}

pub struct MyError {
pub:
	message string
}

pub fn do_work(){
	work := Work{20}
	for i in 0..20 {
		println("working...")
		if i == 15 {
			error := &MyError{"There was an error."}
			eb.publish("error", work, error)
			eb.publish("error", work, error)
			return
		}
	}
}

pub fn get_subscriber() eventbus.Subscriber {
	return *eb.subscriber
}
