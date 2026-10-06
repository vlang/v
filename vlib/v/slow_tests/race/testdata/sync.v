// Translated from Go's src/runtime/race/testdata/sync_test.go, see ../README.md.
import sync
import time

struct Cell[T] {
mut:
	v T
}

fn main() {
	run('test_no_race_cond', test_no_race_cond)
	run('test_race_cond', test_race_cond)
	run('test_race_announce_threads', test_race_announce_threads)
	run('test_no_race_after_func1', test_no_race_after_func1)
	run('test_no_race_after_func2', test_no_race_after_func2)
	run('test_no_race_after_func3', test_no_race_after_func3)
	run('test_race_after_func3', test_race_after_func3)
	run('test_race_goroutine_creation_stack', test_race_goroutine_creation_stack)
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

// after_func is Go's time.AfterFunc: it waits for the duration `d` to elapse and then calls
// `f` in its own thread. The returned Timer can be used to cancel the call with its stop()
// method.
fn after_func(d time.Duration, f fn ()) &sync.Timer {
	timer := sync.new_timer(d)
	spawn fn [timer, f] () {
		_ = <-timer.c or { return }
		f()
	}()
	return timer
}

fn test_no_race_cond() {
	mut x := &Cell[int]{}
	_ = x.v
	mut condition := &Cell[int]{}
	mut mu := sync.new_mutex()
	mut cond := sync.new_cond(mu)
	spawn fn [mut x, mut condition, mut mu, mut cond] () {
		x.v = 1
		mu.lock()
		condition.v = 1
		cond.signal()
		mu.unlock()
	}()
	mu.lock()
	for condition.v != 1 {
		cond.wait()
	}
	mu.unlock()
	x.v = 2
}

fn test_race_cond() {
	done := chan bool{}
	mut mu := sync.new_mutex()
	mut cond := sync.new_cond(mu)
	mut x := &Cell[int]{}
	_ = x.v
	mut condition := &Cell[int]{}
	spawn fn [mut x, mut condition, mut mu, mut cond, done] () {
		time.sleep(10 * time.millisecond) // Enter cond.Wait loop
		x.v = 1
		mu.lock()
		condition.v = 1
		cond.signal()
		mu.unlock()
		time.sleep(10 * time.millisecond) // Exit cond.Wait loop
		mu.lock()
		x.v = 3
		mu.unlock()
		done <- true
	}()
	mu.lock()
	for condition.v != 1 {
		cond.wait()
	}
	mu.unlock()
	x.v = 2
	_ = <-done
}

// We do not currently automatically
// parse this test. It is intended that the creation
// stack is observed manually not to contain
// off-by-one errors
fn test_race_announce_threads() {
	n := 7
	all_done := chan bool{cap: n}

	mut x := &Cell[int]{}
	_ = x.v

	// Go declares `var f, g, h func()` first, so that the closures can refer to each other.
	// V closures capture by value, so they are defined in the order they depend on each other.
	g := fn [mut x, all_done] () {
		for i := 0; i < 2; i++ {
			spawn fn [mut x, all_done] () {
				x.v = 1
				all_done <- true
			}()
			all_done <- true
		}
	}

	f := fn [mut x, all_done, g] () {
		x.v = 1
		spawn g()
		spawn fn [mut x, all_done] () {
			x.v = 1
			all_done <- true
		}()
		x.v = 2
		all_done <- true
	}

	h := fn [mut x, all_done, f] () {
		x.v = 1
		x.v = 2
		spawn f()
		all_done <- true
	}

	spawn h()

	for i := 0; i < n; i++ {
		_ = <-all_done
	}
}

// after_func1_f is the `f` closure of Go's TestNoRaceAfterFunc1, which passes itself to
// time.AfterFunc (a V closure cannot refer to itself).
fn after_func1_f(mut i Cell[int], c chan bool) {
	i.v--
	if i.v >= 0 {
		after_func(0, fn [mut i, c] () {
			after_func1_f(mut i, c)
		})
	} else {
		c <- true
	}
}

fn test_no_race_after_func1() {
	mut i := &Cell[int]{
		v: 2
	}
	c := chan bool{}
	after_func(0, fn [mut i, c] () {
		after_func1_f(mut i, c)
	})
	_ = <-c
}

fn test_no_race_after_func2() {
	mut x := &Cell[int]{}
	_ = x.v
	timer := after_func(10, fn [mut x] () {
		x.v = 1
	})
	defer {
		timer.stop()
	}
}

fn test_no_race_after_func3() {
	c := chan bool{cap: 1}
	mut x := &Cell[int]{}
	_ = x.v
	after_func(10 * time.millisecond, fn [mut x, c] () {
		x.v = 1
		c <- true
	})
	_ = <-c
}

fn test_race_after_func3() {
	c := chan bool{cap: 2}
	mut x := &Cell[int]{}
	_ = x.v
	after_func(10 * time.millisecond, fn [mut x, c] () {
		x.v = 1
		c <- true
	})
	after_func(20 * time.millisecond, fn [mut x, c] () {
		x.v = 2
		c <- true
	})
	_ = <-c
	_ = <-c
}

// This test's output is intended to be
// observed manually. One should check
// that goroutine creation stack is
// comprehensible.
fn test_race_goroutine_creation_stack() {
	mut x := &Cell[int]{}
	_ = x.v
	ch := chan bool{cap: 1}

	f1 := fn [mut x, ch] () {
		x.v = 1
		ch <- true
	}
	f2 := fn [f1] () {
		spawn f1()
	}
	f3 := fn [f2] () {
		spawn f2()
	}
	f4 := fn [f3] () {
		spawn f3()
	}

	spawn f4()
	x.v = 2
	_ = <-ch
}

// Go's TestNoRaceNilMutexCrash is not translated: it checks that calling RLock on a nil
// *sync.RWMutex panics without corrupting the race detector state, and recovers from that
// panic; V has no recover, and a nil pointer dereference is a fatal signal.
