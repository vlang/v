// Translated from Go's src/runtime/race/testdata/time_test.go, see ../README.md.
import time
import sync

struct Cell[T] {
mut:
	v T
}

fn main() {
	run('test_no_race_after_func', test_no_race_after_func)
	run('test_no_race_timer', test_no_race_timer)
	eprintln('=== DONE')
}

// run starts a test like `go test -v` does, so the race reports that follow belong to it.
// Go's harness runs the tests with GOMAXPROCS=1, where the goroutines of a test run as soon
// as it blocks; V threads run in parallel, so give those that outlive the test, and may
// still report a race, a moment to do so before the next test starts.
fn run(name string, test fn ()) {
	eprintln('=== RUN   ${name}')
	test()
	time.sleep(20 * time.millisecond)
}

// after_func is Go's time.AfterFunc, which V's time module does not have: once a timer
// started now fires, f runs in its own thread.
fn after_func(d time.Duration, f fn ()) &sync.Timer {
	t := sync.new_timer(d)
	spawn fn [t, f] () {
		_ = <-t.c
		f()
	}()
	return t
}

fn test_no_race_after_func() {
	mut v := &Cell[int]{}
	_ = v.v
	c := chan int{}
	f := fn [mut v, c] () {
		v.v = 1
		c <- 0
	}
	v.v = 2
	after_func(1, f)
	_ = <-c
	v.v = 3
}

// Go's TestNoRaceAfterFuncReset is not translated: V's sync.Timer has no reset() (and V has
// no time.AfterFunc).

fn test_no_race_timer() {
	mut v := &Cell[int]{}
	_ = v.v
	c := chan int{}
	f := fn [mut v, c] () {
		v.v = 1
		c <- 0
	}
	v.v = 2
	t := sync.new_timer(1)
	spawn fn [t, f] () {
		_ = <-t.c
		f()
	}()
	_ = <-c
	v.v = 3
}

// Go's TestNoRaceTimerReset is not translated: V's sync.Timer has no reset().

// Go's TestNoRaceTicker is not translated: V's time module has no Ticker.

// Go's TestNoRaceTickerReset is not translated: V's time module has no Ticker (and so no
// reset()).
