// Regression test for https://github.com/vlang/v/issues/27205
//
// A generic struct with a `chan T` field, popped through a variable that is
// bound by a map-index `if`-guard (`if w := c.waiters[id] { ... <-w.c.c ... }`).
// The body of such a generic method is generated once per concrete type. The
// channel element type must follow the current instantiation; previously it
// leaked the concrete type of a *different* instantiation, producing invalid C.
module main

import time

struct Inner[T] {
	c chan T
}

struct Waiter[T] {
	c Inner[T]
}

struct Controller[T] {
mut:
	waiters map[int]Waiter[T]
}

fn (mut c Controller[T]) add(id int) {
	c.waiters[id] = Waiter[T]{
		c: Inner[T]{
			c: chan T{cap: 1}
		}
	}
}

fn (mut c Controller[T]) emit(id int, e T) {
	if w := c.waiters[id] {
		w.c.c <- e
	}
}

// bare channel pop through an `if`-guard bound variable
fn (mut c Controller[T]) recv(id int) ?T {
	if w := c.waiters[id] {
		return <-w.c.c
	}
	return none
}

// `select`-based channel pop through an `if`-guard bound variable
fn (mut c Controller[T]) recv_select(id int) ?T {
	if w := c.waiters[id] {
		select {
			r := <-w.c.c {
				return r
			}
			500 * time.millisecond {
				return none
			}
		}
	}
	return none
}

struct EvA {
	a int
}

struct EvB {
	b string
}

fn test_generic_chan_pop_via_if_guard() {
	mut ca := Controller[EvA]{}
	ca.add(0)
	mut cb := Controller[EvB]{}
	cb.add(0)

	ca.emit(0, EvA{ a: 42 })
	cb.emit(0, EvB{ b: 'hello' })

	ra := ca.recv(0) or { EvA{} }
	assert ra.a == 42

	rb := cb.recv(0) or { EvB{} }
	assert rb.b == 'hello'
}

fn test_generic_chan_select_via_if_guard() {
	mut ca := Controller[EvA]{}
	ca.add(0)
	mut cb := Controller[EvB]{}
	cb.add(0)

	ca.emit(0, EvA{ a: 7 })
	cb.emit(0, EvB{ b: 'world' })

	ra := ca.recv_select(0) or { EvA{} }
	assert ra.a == 7

	rb := cb.recv_select(0) or { EvB{} }
	assert rb.b == 'world'
}
