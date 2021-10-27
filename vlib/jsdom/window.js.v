module jsdom

pub struct JS.Window {
}

pub struct Window {
	node JS.Window [noinit]
}

pub fn (elem Window) add_event_listener(event string, cb EventCallback) {
	#elem.node.addEventListener(event.str, function (event) { let e = jsdom__dispatch_event_target(this);
	#let ev = jsdom__dispatch_event(event); ev.event = event;
	#return cb(e,ev)
	#});
}
