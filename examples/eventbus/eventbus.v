module main

import some_module

fn main(){
	mut sub := some_module.get_subscriber()
	sub.subscribe("error", on_error)
	some_module.do_work()
}

fn on_error(_ voidptr, e &some_module.Error, _ voidptr) {
	println(e.message)
}
