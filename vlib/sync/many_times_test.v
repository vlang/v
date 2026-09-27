// vtest build: !windows && (amd64 || arm64)
import sync
import time

struct Counter {
pub mut:
	i int
}

fn (mut c Counter) add(i int) {
	c.i = c.i + i
}

fn run(mut m sync.ManyTimes, mut co Counter, c chan bool) {
	m.do(fn [mut co] () {
		co.add(5)
	})
	c <- true
}

fn test_many_times_once() {
	mut co := &Counter{}
	mut m := sync.new_many_times(1)
	c := chan bool{}
	n := 10

	// It is executed 10 times, but only once actually.
	for i := 0; i < n; i++ {
		spawn run(mut m, mut co, c)
	}
	for i := 0; i < n; i++ {
		<-c
	}
	assert co.i == 5
}

fn test_many_times_fifth() {
	mut co := &Counter{}
	mut m := sync.new_many_times(5)
	c := chan bool{}
	n := 10

	// It is executed 10 times, but only 5 times actually.
	for i := 0; i < n; i++ {
		spawn run(mut m, mut co, c)
	}
	for i := 0; i < n; i++ {
		<-c
	}
	assert co.i == 25
}

// Ordering test: do must not return until f() has completed.
// The bug was that count was stored *before* f() ran, so a concurrent
// caller could observe count >= times and return on the fast path while
// f() was still executing.

struct ManyTimesState {
pub mut:
	value int
	ready chan bool
}

fn run_mt_ordering(mut m sync.ManyTimes, s &ManyTimesState, c chan bool) {
	m.do(fn [s] () {
		mut ms := unsafe { &ManyTimesState(s) }
		ms.ready <- true
		time.sleep(50 * time.millisecond)
		ms.value = 99
	})
	c <- true
}

fn test_many_times_ordering() {
	s := &ManyTimesState{
		ready: chan bool{cap: 1}
	}
	mut m := sync.new_many_times(1)
	c := chan bool{}

	spawn run_mt_ordering(mut m, s, c)

	// Wait until the goroutine is inside f().  With the old buggy code,
	// count is incremented before f() runs, so the second do() call sees
	// count >= times on the fast path and returns immediately — before
	// f() has finished.  With the fix, count is still 0 here, so the
	// second do() blocks on the mutex until f() completes.
	_ := <-s.ready
	m.do(fn () {})

	assert s.value == 99, 'do must not return before f() completes'
	_ := <-c
}
