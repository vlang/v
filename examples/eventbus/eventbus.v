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

fn on_error(sender voidptr, p eventbus.Params) {
	work := *(*some_module.Work(sender))
	println(work.hours)
	println(p.get_string("error"))
}
