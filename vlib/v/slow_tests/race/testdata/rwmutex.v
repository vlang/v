// Translated from Go's src/runtime/race/testdata/rwmutex_test.go, see ../README.md.
import sync
import time

struct Cell[T] {
mut:
	v T
}

fn main() {
	run('test_race_mutex_rw_mutex', test_race_mutex_rw_mutex)
	run('test_no_race_rw_mutex', test_no_race_rw_mutex)
	run('test_race_rw_mutex_multiple_readers', test_race_rw_mutex_multiple_readers)
	run('test_no_race_rw_mutex_multiple_readers', test_no_race_rw_mutex_multiple_readers)
	run('test_no_race_rw_mutex_transitive', test_no_race_rw_mutex_transitive)
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

fn test_race_mutex_rw_mutex() {
	mut mu1 := sync.new_mutex()
	mut mu2 := sync.new_rwmutex()
	mut x := &Cell[i16]{}
	_ = x.v
	ch := chan bool{cap: 2}
	spawn fn [mut mu1, mut x, ch] () {
		mu1.lock()
		defer {
			mu1.unlock()
		}
		x.v = 1
		ch <- true
	}()
	spawn fn [mut mu2, mut x, ch] () {
		mu2.lock()
		x.v = 2
		mu2.unlock()
		ch <- true
	}()
	_ = <-ch
	_ = <-ch
}

fn test_no_race_rw_mutex() {
	mut mu := sync.new_rwmutex()
	mut x := &Cell[i64]{}
	mut y := &Cell[i64]{
		v: 1
	}
	_ = y.v
	ch := chan bool{cap: 2}
	spawn fn [mut mu, mut x, ch] () {
		mu.lock()
		defer {
			mu.unlock()
		}
		x.v = 2
		ch <- true
	}()
	spawn fn [mut mu, mut x, mut y, ch] () {
		mu.rlock()
		y.v = x.v
		mu.runlock()
		ch <- true
	}()
	_ = <-ch
	_ = <-ch
}

fn test_race_rw_mutex_multiple_readers() {
	mut mu := sync.new_rwmutex()
	mut x := &Cell[i64]{}
	mut y := &Cell[i64]{
		v: 1
	}
	ch := chan bool{cap: 4}
	spawn fn [mut mu, mut x, ch] () {
		mu.lock()
		defer {
			mu.unlock()
		}
		x.v = 2
		ch <- true
	}()
	// Use three readers so that no matter what order they're
	// scheduled in, two will be on the same side of the write
	// lock above.
	spawn fn [mut mu, mut x, mut y, ch] () {
		mu.rlock()
		y.v = x.v + 1
		mu.runlock()
		ch <- true
	}()
	spawn fn [mut mu, mut x, mut y, ch] () {
		mu.rlock()
		y.v = x.v + 2
		mu.runlock()
		ch <- true
	}()
	spawn fn [mut mu, mut x, mut y, ch] () {
		mu.rlock()
		y.v = x.v + 3
		mu.runlock()
		ch <- true
	}()
	_ = <-ch
	_ = <-ch
	_ = <-ch
	_ = <-ch
	_ = y.v
}

fn test_no_race_rw_mutex_multiple_readers() {
	mut mu := sync.new_rwmutex()
	mut x := &Cell[i64]{}
	ch := chan bool{cap: 4}
	spawn fn [mut mu, mut x, ch] () {
		mu.lock()
		defer {
			mu.unlock()
		}
		x.v = 2
		ch <- true
	}()
	spawn fn [mut mu, mut x, ch] () {
		mu.rlock()
		y := x.v + 1
		_ = y
		mu.runlock()
		ch <- true
	}()
	spawn fn [mut mu, mut x, ch] () {
		mu.rlock()
		y := x.v + 2
		_ = y
		mu.runlock()
		ch <- true
	}()
	spawn fn [mut mu, mut x, ch] () {
		mu.rlock()
		y := x.v + 3
		_ = y
		mu.runlock()
		ch <- true
	}()
	_ = <-ch
	_ = <-ch
	_ = <-ch
	_ = <-ch
}

fn test_no_race_rw_mutex_transitive() {
	mut mu := sync.new_rwmutex()
	mut x := &Cell[i64]{}
	ch := chan bool{cap: 2}
	spawn fn [mut mu, mut x, ch] () {
		mu.rlock()
		_ = x.v
		mu.runlock()
		ch <- true
	}()
	spawn fn [mut mu, mut x, ch] () {
		time.sleep(10 * time.millisecond)
		mu.rlock()
		_ = x.v
		mu.runlock()
		ch <- true
	}()
	time.sleep(20 * time.millisecond)
	mu.lock()
	x.v = 42
	mu.unlock()
	_ = <-ch
	_ = <-ch
}
