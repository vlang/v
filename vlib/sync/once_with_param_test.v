import sync
import time

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
	}, o)
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
	value int
	ready chan bool
}

fn slow_init(p voidptr) {
	mut s := unsafe { &OrderingState(p) }
	// Signal that we are inside f() — count has been stored (buggy) or not (fixed).
	s.ready <- true
	// Sleep long enough for the concurrent do_with_param call to observe the state.
	time.sleep(50 * time.millisecond)
	s.value = 99
}

fn run_ordering(mut once sync.Once, s &OrderingState, c chan bool) {
	once.do_with_param(slow_init, s)
	c <- true
}

fn test_once_with_param_ordering() {
	mut s := &OrderingState{
		ready: chan bool{cap: 1}
	}
	mut once := sync.new_once()
	c := chan bool{}

	spawn run_ordering(mut once, s, c)

	// Wait until the goroutine is inside f() before letting the second
	// caller proceed.  With the old buggy code count==1 is already visible
	// here, so the second do_with_param returns immediately — before
	// s.value is set.  With the fix, count is still 0, so the second
	// caller blocks on the mutex and waits for f() to complete.
	_ := <-s.ready
	once.do_with_param(fn (p voidptr) {}, unsafe { nil })

	assert s.value == 99, 'do_with_param must not return before f() completes'
	_ := <-c
}
