module main

pub type EventListener[T] = fn (T) !

pub type Check[T] = fn (T) bool

pub struct EventController[T] {
mut:
	id        int
	listeners map[int]EventListener[T]
}

fn (mut ec EventController[T]) generate_id() int {
	return ec.id++
}

pub fn (mut ec EventController[T]) override(listener EventListener[T]) EventController[T] {
	ec.listeners = {
		ec.generate_id(): listener
	}
	return ec
}

fn use[T](_ EventController[T]) {}

struct Foo {}

struct Bar {}

fn test_main() {
	use(EventController[Foo]{})
	use(EventController[Bar]{})
}
