module some_module

import (
	eventbus
)

const (
	eb = eventbus.new()
)

pub fn do_work(){
	mut params := eventbus.Params{}
	for i in 0..20 {
		println("working...")
		if i == 15 {
			params.put_string("error", "CRASH!!")
			eb.publish("error", params)
			eb.publish("error", params)
			return
		}
	}
	
}

pub fn get_subscriber() eventbus.Subscriber {
	return eb.subscriber
}
