// vtest build: !windows && (amd64 || arm64)
import sync
import sync.stdatomic

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
	value   u64
	ready   chan bool
	release chan bool
}

fn run_mt_ordering(mut m sync.ManyTimes, mut state ManyTimesState) {
	m.do(fn [mut state] () {
		state.ready <- true
		_ := <-state.release
		stdatomic.store_u64(&state.value, 99)
	})
}

fn test_many_times_ordering() {
	mut state := &ManyTimesState{
		ready:   chan bool{}
		release: chan bool{}
	}
	mut m := sync.new_many_times(1)
	worker := spawn run_mt_ordering(mut m, mut state)

	_ := <-state.ready
	published := stdatomic.load_u64(&m.count)
	state.release <- true
	m.do(fn () {
		assert false, 'the second call must not execute another callback'
	})
	value := stdatomic.load_u64(&state.value)
	worker.wait()
	assert published == 0, 'do must publish the count after the callback completes'
	assert value == 99, 'do must not return before the callback completes'
}
