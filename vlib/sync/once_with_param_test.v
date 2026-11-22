import sync
import sync.stdatomic

// Note: this is the same test as `vlib/sync/once_test.v`, but
// it uses an explicit passing of the voidptr parameter in
// once.do_with_param/2, instead of passing a closure of it
// in once.do/1.

struct One {
pub mut:
	i int
}

fn (mut o One) add(i int) {
	o.i = o.i + i
}

fn run(mut once sync.Once, mut o One, c chan bool) {
	once.do_with_param(fn (mut o One) {
		o.add(5)
	}, &o)
	c <- true
}

fn test_once() {
	mut o := &One{}
	mut once := sync.new_once()
	c := chan bool{}
	n := 10

	// It is executed 10 times, but only once actually.
	for i := 0; i < n; i++ {
		spawn run(mut once, mut o, c)
	}
	for i := 0; i < n; i++ {
		<-c
	}
	assert o.i == 5
}

// Ordering test: do_with_param must not return until f() has completed.
// The bug was that count was set to 1 *before* f() ran, so a concurrent
// caller could observe count==1 and return on the fast path while f() was
// still executing (or hadn't started).

struct OrderingState {
pub mut:
	value   u64
	ready   chan bool
	release chan bool
}

fn gated_init(p voidptr) {
	// do_with_param passes the fixture state as an opaque pointer.
	mut s := unsafe { &OrderingState(p) }
	s.ready <- true
	_ := <-s.release
	stdatomic.store_u64(&s.value, 99)
}

fn run_ordering(mut once sync.Once, s &OrderingState) {
	once.do_with_param(gated_init, s)
}

@[unsafe]
fn test_once_with_param_ordering() {
	s := &OrderingState{
		ready:   chan bool{}
		release: chan bool{}
	}
	mut once := sync.new_once()
	worker := spawn run_ordering(mut once, s)

	_ := <-s.ready
	published := stdatomic.load_u64(&once.count)
	s.release <- true
	once.do_with_param(fn (_ voidptr) {
		assert false, 'the second call must not execute another callback'
	}, s)
	value := stdatomic.load_u64(&s.value)
	worker.wait()
	assert published == 0, 'do_with_param must publish completion after the callback'
	assert value == 99, 'do_with_param must not return before the callback completes'
}
