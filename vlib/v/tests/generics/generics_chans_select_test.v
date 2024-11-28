import time

pub type EventListener[T] = fn (T) !

@[heap]
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
pub struct EmitOptions {
pub:
	error_handler ?fn (int, IError)
}

pub fn (mut ec EventController[T]) emit(e T, options EmitOptions) {
	for i, w in ec.wait_fors {
		mut b := false
		if w.check != none {
			func := w.check
			b = func(e)
		} else {
			assert false
		}
		if b {
			w.c.c <- e
			ec.wait_fors.delete(i)
			return
		}
	}
	mut ts := []thread{}
	for i, l in ec.listeners {
		ts << spawn fn [options] [T](f EventListener[T], j int, e T) {
			f(e) or {
				if g := options.error_handler {
					g(j, err)
				}
			}
		}(l, i, e)
	}
	ts.wait()
}

@[params]
pub struct EventWaitParams[T] {
pub:
	check   ?Check[T]
	timeout ?time.Duration
}

pub struct Awaitable[T] {
	id      int
	timeout ?time.Duration
	c       Chan[T]
mut:
	controller &EventController[T]
}

struct Zero[T] {
	e T
}

pub fn (mut a Awaitable[T]) do() ?T {
	defer {
		a.controller.wait_fors.delete(a.id)
	}
	if timeout := a.timeout {
		mut e := Zero[T]{}.e
		select {
			e = <-a.c.c {
				r := e
				return r
			}
			timeout.nanoseconds() {
				return none
			}
		}
	}
	return <-a.c.c
}

pub fn (mut ec EventController[T]) wait(params EventWaitParams[T]) Awaitable[T] {
	mut c := Chan[T]{}
	id := ec.generate_id()
	ec.wait_fors[id] = EventWaiter[T]{
		check: params.check
		c:     &mut c
	}
	return Awaitable[T]{
		id:         id
		timeout:    params.timeout
		controller: unsafe { &mut ec }
		c:          c
	}
}

struct Foo {}

struct Bar {}

fn test_generics_chans_select() {
	x := EventController[Foo]{}
	println(x)
	y := EventController[Bar]{}
	println(y)
	assert true
}
