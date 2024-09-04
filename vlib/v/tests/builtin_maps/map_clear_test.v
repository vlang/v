pub type EventListener[T] = fn (T) !

pub struct EventController[T] {
mut:
	id        int
	listeners map[int]EventListener[T]
}

fn (mut ec EventController[T]) generate_id() int {
	return ec.id++
}

pub fn (mut ec EventController[T]) override(listener EventListener[T]) EventController[T] {
	ec.listeners.clear()
	return ec.listen(listener)
}

pub fn (mut ec EventController[T]) listen(listener EventListener[T]) EventController[T] {
	ec.listeners[ec.generate_id()] = listener
	return ec
}

struct Foo {}

struct Bar {}

fn make[T](i int) EventController[T] {
	return EventController[T]{
		id: i
	}
}

fn test_main() {
	mut a := EventController[Foo]{
		id:        1
		listeners: {
			1: fn (a Foo) ! {
				dump(1)
			}
		}
	}
	assert dump(a).id == 1
	assert dump(a).listeners.len == 1
	a.override(fn (a Foo) ! {
		dump(2)
	})
	assert dump(a).id == 2
	assert dump(a).listeners.len == 1
}
