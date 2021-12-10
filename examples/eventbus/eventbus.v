module main

import some_module

fn main() {
	mut sub := some_module.get_subscriber()
	sub.subscribe('error', on_error)
	some_module.do_work()
}

fn on_error(sender voidptr, e &some_module.MyError, x voidptr) {
	println(e.message)
}
