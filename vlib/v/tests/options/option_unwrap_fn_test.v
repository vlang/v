@[heap]
struct Chan[T] {
	c chan T
}

pub type EventListener[T] = fn (T) !

pub type Check[T] = fn (T) bool

struct EventWaiter[T] {
	check ?Check[T]
	c     Chan[T]
}

pub struct EventController[T] {
mut:
	id        int
	wait_fors map[int]EventWaiter[T]
	listeners map[int]EventListener[T]
}

fn (mut ec EventController[T]) generate_id() int {
	return ec.id++
}

pub fn (ec EventController[T]) emit(e T) bool {
	for i, w in ec.wait_fors {
		mut b := false
		c := w.check
		if c != none {
			b = c(e)
		} else {
			b = true
		}
		println('${i} passed: ${b}')
		return b
	}
	return false
}

struct Foo {}

struct Bar {}

fn use[T](z EventController[T]) bool {
	return z.emit(T{})
}

fn test_main() {
	assert use(EventController[Foo]{
		wait_fors: {
			0: EventWaiter[Foo]{
				check: fn (t Foo) bool {
					return false
				}
			}
		}
	}) == false
	assert use(EventController[Bar]{
		wait_fors: {
			0: EventWaiter[Bar]{}
		}
	}) == true
}
