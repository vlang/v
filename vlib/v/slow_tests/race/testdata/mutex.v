// Translated from Go's src/runtime/race/testdata/mutex_test.go, see ../README.md.
import sync
import time

struct Cell[T] {
mut:
	v T
}

fn main() {
	run('test_no_race_mutex', test_no_race_mutex)
	run('test_race_mutex', test_race_mutex)
	run('test_race_mutex2', test_race_mutex2)
	run('test_no_race_mutex_pure_happens_before', test_no_race_mutex_pure_happens_before)
	run('test_no_race_mutex_example_from_html', test_no_race_mutex_example_from_html)
	run('test_race_mutex_overwrite', test_race_mutex_overwrite)
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

fn test_no_race_mutex() {
	mut mu := sync.new_mutex()
	mut x := &Cell[i16]{}
	ch := chan bool{cap: 2}
	spawn fn [mut mu, mut x, ch] () {
		mu.lock()
		defer {
			mu.unlock()
		}
		x.v = 1
		ch <- true
	}()
	spawn fn [mut mu, mut x, ch] () {
		mu.lock()
		x.v = 2
		mu.unlock()
		ch <- true
	}()
	_ = <-ch
	_ = <-ch
}

fn test_race_mutex() {
	mut mu := sync.new_mutex()
	mut x := &Cell[i16]{}
	ch := chan bool{cap: 2}
	spawn fn [mut mu, mut x, ch] () {
		x.v = 1
		mu.lock()
		defer {
			mu.unlock()
		}
		ch <- true
	}()
	spawn fn [mut mu, mut x, ch] () {
		x.v = 2
		mu.lock()
		mu.unlock()
		ch <- true
	}()
	_ = <-ch
	_ = <-ch
}

fn test_race_mutex2() {
	mut mu1 := sync.new_mutex()
	mut mu2 := sync.new_mutex()
	mut x := &Cell[i8]{}
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

fn test_no_race_mutex_pure_happens_before() {
	mut mu := sync.new_mutex()
	mut x := &Cell[i16]{}
	mut written := &Cell[bool]{}
	ch := chan bool{cap: 2}
	spawn fn [mut mu, mut x, mut written, ch] () {
		x.v = 1
		mu.lock()
		written.v = true
		mu.unlock()
		ch <- true
	}()
	spawn fn [mut mu, mut x, mut written, ch] () {
		time.sleep(100 * time.microsecond)
		mu.lock()
		for !written.v {
			mu.unlock()
			time.sleep(100 * time.microsecond)
			mu.lock()
		}
		mu.unlock()
		x.v = 1
		ch <- true
	}()
	_ = <-ch
	_ = <-ch
}

// Go's TestNoRaceMutexSemaphore unlocks the mutex in a goroutine other than the one that
// locked it. V's sync.Mutex is a pthread mutex, for which that is undefined behaviour, so
// the test is not translated.

// from doc/go_mem.html. Go's version unlocks `l` in the spawned goroutine, which a pthread
// mutex does not allow; a semaphore expresses the same handoff in V.
fn test_no_race_mutex_example_from_html() {
	mut l := sync.new_semaphore()
	mut a := &Cell[string]{}
	spawn fn [mut l, mut a] () {
		a.v = 'hello, world'
		l.post()
	}()
	l.wait()
	_ = a.v
}

fn test_race_mutex_overwrite() {
	c := chan bool{cap: 1}
	mut mu := sync.new_mutex()
	spawn fn [mut mu, c] () {
		unsafe {
			*mu = sync.Mutex{}
		}
		c <- true
	}()
	mu.lock()
	_ = <-c
}
