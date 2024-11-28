struct Registry[T] {
mut:
	events []EventHandler[T]
}

struct EventHandler[T] {
	name T
}

fn (mut r Registry[T]) test() {
	mut events := []EventHandler[string]{}
	r.events << EventHandler[string]{
		name: 'test'
	}
	assert 1 == r.events.len

	r.events.clear()
	events.clear()

	assert events.len == r.events.len
}

fn test_main() {
	mut registry := &Registry[string]{
		events: []
	}
	registry.test()
	assert dump(registry.events).len == 0
}
