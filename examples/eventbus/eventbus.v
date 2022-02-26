module main

import some_module

struct Receiver {
mut:
	ok bool
}

fn main() {
	mut sub := some_module.get_subscriber()
	r := Receiver{}
	sub.subscribe_method('event_foo', on_foo, r)
	sub.subscribe('event_bar', on_bar)
	sub.subscribe('event_baz', on_baz)

	println('Receiver ok: ' + r.ok.str())
	some_module.do_work()
	println('Receiver ok: ' + r.ok.str())
}

fn on_foo(mut receiver Receiver, sender voidptr, e &some_module.EventMetadata) {
	receiver.ok = true
	println('on_foo :: ' + e.message)
}

fn on_bar(receiver voidptr, sender voidptr, e &some_module.EventMetadata) {
	println('on_bar :: ' + e.message)
}

fn on_baz(receiver voidptr, d &some_module.Duration, e voidptr) {
	println('on_baz :: ' + d.hours.str())
}
