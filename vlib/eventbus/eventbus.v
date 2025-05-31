module eventbus

pub type EventHandlerFn = fn (receiver voidptr, args voidptr, sender voidptr)

pub struct Publisher[T] {
mut:
	registry &Registry[T] = unsafe { nil }
}

pub struct Subscriber[T] {
mut:
	registry &Registry[T] = unsafe { nil }
}

struct Registry[T] {
mut:
	events []EventHandler[T]
}

struct EventHandler[T] {
	name     T
	handler  EventHandlerFn = unsafe { nil }
	receiver voidptr        = unsafe { nil }
	once     bool
}

pub struct EventBus[T] {
pub mut:
	registry   &Registry[T]   = unsafe { nil }
	publisher  &Publisher[T]  = unsafe { nil }
	subscriber &Subscriber[T] = unsafe { nil }
}

// EventBus.new[T] create a new eventbus with event type T.
pub fn EventBus.new[T]() &EventBus[T] {
	registry := &Registry[T]{
		events: []
	}
	return &EventBus[T]{registry, &Publisher[T]{registry}, &Subscriber[T]{registry}}
}

// new[T] create a new eventbus with event type T.
pub fn new[T]() &EventBus[T] {
	registry := &Registry[T]{
		events: []
	}
	return &EventBus[T]{registry, &Publisher[T]{registry}, &Subscriber[T]{registry}}
}

// publish publishes an event with provided Params & name.
pub fn (eb &EventBus[T]) publish(name T, sender voidptr, args voidptr) {
	mut publisher := eb.publisher
	publisher.publish(name, sender, args)
}

// clear_all clears all subscribers.
pub fn (eb &EventBus[T]) clear_all() {
	mut publisher := eb.publisher
	publisher.clear_all()
}

// has_subscriber check if a subscriber to an event exists.
pub fn (eb &EventBus[T]) has_subscriber(name T) bool {
	return eb.registry.check_subscriber(name)
}

const dedup_buffer_len = 20

// publish publish an event with provided Params & name.
fn (mut pb Publisher[T]) publish(name T, sender voidptr, args voidptr) {
	// println('Publisher.publish(name=${name} sender=${sender} args=${args})')
	mut handled_receivers := unsafe { [dedup_buffer_len]voidptr{} } // handle duplicate bugs TODO fix properly + perf
	// is_key_down := name == 'on_key_down'
	mut j := 0
	for event in pb.registry.events {
		if event.name == name {
			// if is_key_down {
			if event.receiver in handled_receivers {
				continue
			}
			//}
			// println('got ${i + 1} name=${name} event.receiver=${event.receiver}')
			event.handler(event.receiver, args, sender)
			// handled_receivers << event.receiver
			handled_receivers[j] = event.receiver
			j = (j + 1) % dedup_buffer_len
		}
	}
	pb.registry.events = pb.registry.events.filter(!(it.name == name && it.once))
}

// clear_all clear all subscribers.
fn (mut p Publisher[T]) clear_all() {
	p.registry.events.clear()
}

// subscribe subscribe to an event `name`.
pub fn (mut s Subscriber[T]) subscribe(name T, handler EventHandlerFn) {
	s.registry.events << EventHandler[T]{
		name:    name
		handler: handler
	}
}

// subscribe_method subscribe to an event `name` and also set the `receiver` as a parameter.
pub fn (mut s Subscriber[T]) subscribe_method(name T, handler EventHandlerFn, receiver voidptr) {
	s.registry.events << EventHandler[T]{
		name:     name
		handler:  handler
		receiver: receiver
	}
}

// unsubscribe_method unsubscribe a receiver for only one method.
pub fn (mut s Subscriber[T]) unsubscribe_method(name T, receiver voidptr) {
	s.registry.events = s.registry.events.filter(!(it.name == name && it.receiver == receiver))
}

// unsubscribe_receiver unsubscribes a receiver from all events.
pub fn (mut s Subscriber[T]) unsubscribe_receiver(receiver voidptr) {
	s.registry.events = s.registry.events.filter(it.receiver != receiver)
}

// subscribe_once subscribe only once to an event `name`.
pub fn (mut s Subscriber[T]) subscribe_once(name T, handler EventHandlerFn) {
	s.registry.events << EventHandler[T]{
		name:    name
		handler: handler
		once:    true
	}
}

// is_subscribed check if we are subscribed to an event `name`.
pub fn (s &Subscriber[T]) is_subscribed(name T) bool {
	return s.registry.check_subscriber(name)
}

// is_subscribed_method checks whether a receiver was already subscribed for any events.
pub fn (s &Subscriber[T]) is_subscribed_method(name T, receiver voidptr) bool {
	return s.registry.events.any(it.name == name && it.receiver == receiver)
}

// unsubscribe unsubscribe from an event `name`.
pub fn (mut s Subscriber[T]) unsubscribe(name T, handler EventHandlerFn) {
	// v := voidptr(handler)
	s.registry.events = s.registry.events.filter(!(it.name == name && it.handler == handler))
}

// Registry Methods
fn (r &Registry[T]) check_subscriber(name T) bool {
	return r.events.any(it.name == name)
}
