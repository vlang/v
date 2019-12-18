module eventbus

pub struct Publisher {
	mut:
	registry &Registry
}

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
	func fn(voidptr, Params)
}

pub struct EventBus{
	pub mut:
	registry &Registry
	publisher &Publisher
	pub:
	subscriber &Subscriber
}

pub fn new() &EventBus{
	registry := &Registry{
		names: []
		events: []
		once: []
	}
	return &EventBus{
		registry,
		&Publisher{registry},
		&Subscriber{registry}
	}
}

// EventBus Methods

pub fn (eb &EventBus) publish(name string, sender voidptr, p Params) {
	mut publisher := eb.publisher
	publisher.publish(name, sender, p)
}

pub fn (eb &EventBus) clear_all(){
	mut publisher := eb.publisher
	publisher.clear_all()
}

pub fn (eb &EventBus) has_subscriber(name string) bool {
	return eb.registry.check_subscriber(name)
}

// Publisher Methods

fn (pb mut Publisher) publish(name string, sender voidptr, p Params){
	//p.put_custom("sender", "any", sender) //add sender to params
	for i, n in pb.registry.names {
		if name == n {
			eh := pb.registry.events[i]
			once_index := pb.registry.once.index(pb.registry.names[i])
			if once_index > -1 {
				pb.registry.events.delete(i)
				pb.registry.names.delete(i)
				pb.registry.once.delete(once_index)
			}
			invoke(eh, sender, p)
		}
	}
}

fn (p mut Publisher) clear_all(){
	if p.registry.names.len == 0 {return}
	for i := p.registry.names.len - 1; i >= 0; i-- {
		p.registry.delete_entry(i)
	}
}

// Subscriber Methods

pub fn (s mut Subscriber) subscribe(name string, handler fn(voidptr, Params)){
	s.registry.names << name
	v := voidptr(handler)
	s.registry.events << v
}

pub fn (s mut Subscriber) subscribe_once(name string, handler fn(voidptr, Params)){
	s.subscribe(name, handler)
	s.registry.once << name
}

pub fn (s &Subscriber) is_subscribed(name string) bool {
	return s.registry.check_subscriber(name)
}

pub fn (s mut Subscriber) unsubscribe(name string, handler fn(voidptr, Params)){
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

fn invoke(p, sender voidptr, arr Params){
	handler := EventHandler{p}.func
	handler(sender, arr)
}
