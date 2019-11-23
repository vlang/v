module eventbus

pub struct Subscriber {
	mut:
	registry &Registry
}
struct Registry{
	mut:
	names []string
	events []voidptr
	once []string
}

struct EventHandler {
	func fn(Params)
}

pub struct EventBus{
	mut:
	registry &Registry
	pub:
	subscriber Subscriber
}

// EventBus Methods

pub fn new() &EventBus{
	registry := &Registry{
		names: []
		events: []
		once: []
	}
	return &EventBus{
		registry,
		Subscriber{registry}
	}
}

pub fn (eb mut EventBus) publish(name string, p Params){
	for i, n in eb.registry.names {
		if name == n {
			eh := eb.registry.events[i]
			invoke(eh, p)
			once_index := eb.registry.once.index(eb.registry.names[i])
			if once_index > -1 {
				eb.registry.events.delete(i)
				eb.registry.names.delete(i)
				eb.registry.once.delete(once_index)
			}
		}
	}
}

pub fn (eb mut EventBus) clear_all(){
	for i, n in eb.registry.names {
		eb.registry.delete_entry(i)
	}
}

pub fn (eb &EventBus) has_subscriber(name string) bool {
	return eb.registry.check_subscriber(name)
}

// Subscriber Methods

pub fn (s mut Subscriber) subscribe(name string, handler fn(Params)){
	s.registry.names << name
	v := voidptr(handler)
	s.registry.events << v
}

pub fn (s mut Subscriber) subscribe_once(name string, handler fn(Params)){
	s.subscribe(name, handler)
	s.registry.once << name
}

pub fn (s &Subscriber) is_subscribed(name string) bool {
	return s.registry.check_subscriber(name)
}

pub fn (s mut Subscriber) unsubscribe(name string, handler fn(Params)){
	v := voidptr(handler)
	for i, n in s.registry.names {
		if name == n {
			eh := s.registry.events[i]
			if eh == v {
				s.registry.delete_entry(i)
			}
		}
	}
}

// Registry Methods

fn (r &Registry) check_subscriber(name string) bool {
	for n in r.names {
		if name == n {return true}
	}
	return false
}

fn (r mut Registry) delete_entry(index int) {
	once_index := r.once.index(r.names[index])
	if once_index > -1 {
		r.once.delete(once_index)
	}
	r.events.delete(index)
	r.names.delete(index)
}

// Helper Functions

fn invoke(p voidptr, arr Params){
	handler := EventHandler{p}.func
	handler(arr)
}