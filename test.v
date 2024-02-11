import time

pub type EventListener[T] = fn (T) !

struct Chan[T] {
	c chan T
}

pub type Check[T] = fn (T) bool

struct EventWaiter[T] {
	check ?Check[T]
	c     &Chan[T]
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


@[params]
pub struct EventWaitParams[T] {
pub:
	check   ?Check[T]
	timeout ?time.Duration
}

pub fn (mut ec EventController[T]) wait(params EventWaitParams[T]) ?T {
	mut c := Chan[T]{}
	id := ec.generate_id()
	ec.wait_fors[id] = EventWaiter[T]{
		check: params.check
		c: &mut c
	}
	defer {
		ec.wait_fors.delete(id)
	}
	if timeout := params.timeout {
		select {
			e := <-c.c {
				r := e
				return r
			}
			timeout.nanoseconds() {
				return none
			}
		}
	}
	return <-c.c
}

struct Foo {}
struct Bar {}

fn main() {
	x := EventController[Foo]{}
	y := EventController[Bar]{}
}