struct Store[Z] {
mut:
	event_handlers []IEventHandler[Z]
}

fn (mut me Store[Z]) add_event_handler(event_handler IEventHandler[Z]) {
	me.event_handlers << IEventHandler[Z](event_handler)
}

interface IEventHandler[Z] {
mut:
	apply(event Z)
}

struct UserEvent {}

struct UserEventHandler {}

pub fn (mut me UserEventHandler) apply(event UserEvent) {}

fn test_generic_interface_with_method_using_generic_struct() {
	mut s := Store[UserEvent]{}
	s.add_event_handler(UserEventHandler{})
	assert s.event_handlers.len > 0
}
