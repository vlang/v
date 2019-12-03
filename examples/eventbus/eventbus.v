module main

import (
	some_module
	eventbus
)

fn main(){
	mut sub := some_module.get_subscriber()
	sub.subscribe("error", on_error)
	some_module.do_work()
}

fn on_error(p eventbus.Params) {
	println(p.get_string("error"))
}
