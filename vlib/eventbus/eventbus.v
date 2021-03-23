module eventbus

pub type EventHandlerFn = fn (receiver voidptr, args voidptr, sender voidptr)

pub struct Publisher {
mut:
	registry &Registry
}

pub struct Subscriber {
mut:
	registry &Registry
}

struct Registry {
mut:
	events []EventHandler
}

struct EventHandler {
	name     string
	handler  EventHandlerFn
	receiver voidptr = voidptr(0)
	once     bool
}

pub struct EventBus {
pub mut:
	registry   &Registry
	publisher  &Publisher
	subscriber &Subscriber
}

pub fn new() &EventBus {
	registry := &Registry{
		events: []
	}
	return &EventBus{registry, &Publisher{registry}, &Subscriber{registry}}
}

// EventBus Methods
pub fn (eb &EventBus) publish(name string, sender voidptr, args voidptr) {
	mut publisher := eb.publisher
	publisher.publish(name, sender, args)
}

pub fn (eb &EventBus) clear_all() {
	mut publisher := eb.publisher
	publisher.clear_all()
}

pub fn (eb &EventBus) has_subscriber(name string) bool {
	return eb.registry.check_subscriber(name)
}

// Publisher Methods
fn (mut pb Publisher) publish(name string, sender voidptr, args voidptr) {
	for event in pb.registry.events {
		if event.name == name {
			event.handler(event.receiver, args, sender)
		}
	}
	pb.registry.events = pb.registry.events.filter(!(it.name == name && it.once))
}

fn (mut p Publisher) clear_all() {
	p.registry.events.clear()
}

// Subscriber Methods
pub fn (mut s Subscriber) subscribe(name string, handler EventHandlerFn) {
	s.registry.events << EventHandler{
		name: name
		handler: handler
	}
}

pub fn (mut s Subscriber) subscribe_method(name string, handler EventHandlerFn, receiver voidptr) {
	s.registry.events << EventHandler{
		name: name
		handler: handler
		receiver: receiver
	}
}

// unsubscribe_method unsubscribe a receiver for only one method 
pub fn (mut s Subscriber) unsubscribe_method(name string, receiver voidptr) {
	s.registry.events = s.registry.events.filter(!(it.name == name && it.receiver == receiver))
}

// unsubscribe_receiver unsubscribes a receiver from all events
pub fn (mut s Subscriber) unsubscribe_receiver(receiver voidptr) {
	s.registry.events = s.registry.events.filter(it.receiver != receiver)
}

pub fn (mut s Subscriber) subscribe_once(name string, handler EventHandlerFn) {
	s.registry.events << EventHandler{
		name: name
		handler: handler
		once: true
	}
}

pub fn (s &Subscriber) is_subscribed(name string) bool {
	return s.registry.check_subscriber(name)
}



// is_subscribed_method checks whether a receiver was already subscribed for any events
pub fn (s &Subscriber) is_subscribed_method(name string, receiver voidptr) bool {
	return s.registry.events.any(it.name == name && it.receiver == receiver)
}

pub fn (mut s Subscriber) unsubscribe(name string, handler EventHandlerFn) {
	// v := voidptr(handler)
	s.registry.events = s.registry.events.filter(!(it.name == name && it.handler == handler))
}

// Registry Methods
fn (r &Registry) check_subscriber(name string) bool {
	return r.events.any(it.name == name)
}
