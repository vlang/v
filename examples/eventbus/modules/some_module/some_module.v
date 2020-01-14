module some_module

import (
	eventbus
)

const (
	eb = eventbus.new()
)

pub struct Work {
	pub:
	hours int
}

pub fn do_work(){
	work := Work{20}
	mut params := eventbus.Params{}
	for i in 0..20 {
		println("working...")
		if i == 15 {
			params.put_string("error", "CRASH!!")
			eb.publish("error", work, params)
			eb.publish("error", work, params)
			return
		}
	}
	
}

pub fn get_subscriber() eventbus.Subscriber {
	return eb.subscriber
}
