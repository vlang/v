// Translated from Go's src/runtime/race/testdata/waitgroup_test.go, see ../README.md.
import sync
import time

struct Cell[T] {
mut:
	v T
}

// The `const P = 3` and `const T = 3` of the Go tests (V has no function-local constants).
// The `[P]int` arrays are written as `[3]int`: V3 generates broken C for `Cell[[p_count]int]`.
const p_count = 3
const t_count = 3

fn main() {
	run('test_no_race_wait_group', test_no_race_wait_group)
	run('test_race_wait_group', test_race_wait_group)
	run('test_no_race_wait_group2', test_no_race_wait_group2)
	run('test_race_wait_group_as_mutex', test_race_wait_group_as_mutex)
	run('test_race_wait_group_wrong_wait', test_race_wait_group_wrong_wait)
	run('test_race_wait_group_wrong_add', test_race_wait_group_wrong_add)
	run('test_no_race_wait_group_multiple_wait', test_no_race_wait_group_multiple_wait)
	run('test_no_race_wait_group_multiple_wait2', test_no_race_wait_group_multiple_wait2)
	run('test_no_race_wait_group_multiple_wait3', test_no_race_wait_group_multiple_wait3)
	run('test_race_wait_group2', test_race_wait_group2)
	run('test_no_race_wait_group_panic_recover', test_no_race_wait_group_panic_recover)
	run('test_no_race_wait_group_panic_recover2', test_no_race_wait_group_panic_recover2)
	run('test_no_race_wait_group_transitive', test_no_race_wait_group_transitive)
	run('test_no_race_wait_group_reuse', test_no_race_wait_group_reuse)
	run('test_no_race_wait_group_reuse2', test_no_race_wait_group_reuse2)
	run('test_race_wait_group_reuse', test_race_wait_group_reuse)
	run('test_no_race_wait_group_concurrent_add', test_no_race_wait_group_concurrent_add)
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

fn test_no_race_wait_group() {
	mut x := &Cell[int]{}
	_ = x.v
	mut wg := sync.new_waitgroup()
	n := 1
	for i := 0; i < n; i++ {
		wg.add(1)
		j := i
		spawn fn [mut x, mut wg, j] () {
			x.v = j
			wg.done()
		}()
	}
	wg.wait()
}

fn test_race_wait_group() {
	mut x := &Cell[int]{}
	_ = x.v
	mut wg := sync.new_waitgroup()
	n := 2
	for i := 0; i < n; i++ {
		wg.add(1)
		j := i
		spawn fn [mut x, mut wg, j] () {
			x.v = j
			wg.done()
		}()
	}
	wg.wait()
}

fn test_no_race_wait_group2() {
	mut x := &Cell[int]{}
	_ = x.v
	mut wg := sync.new_waitgroup()
	wg.add(1)
	spawn fn [mut x, mut wg] () {
		x.v = 1
		wg.done()
	}()
	wg.wait()
	x.v = 2
}

// incrementing counter in Add and locking wg's mutex
fn test_race_wait_group_as_mutex() {
	mut x := &Cell[int]{}
	_ = x.v
	mut wg := sync.new_waitgroup()
	c := chan bool{cap: 2}
	spawn fn [mut x, mut wg, c] () {
		wg.wait()
		time.sleep(100 * time.millisecond)
		wg.add(+1)
		x.v = 1
		wg.add(-1)
		c <- true
	}()
	spawn fn [mut x, mut wg, c] () {
		wg.wait()
		time.sleep(100 * time.millisecond)
		wg.add(+1)
		x.v = 2
		wg.add(-1)
		c <- true
	}()
	_ = <-c
	_ = <-c
}

// Incorrect usage: Add is too late.
fn test_race_wait_group_wrong_wait() {
	c := chan bool{cap: 2}
	mut x := &Cell[int]{}
	_ = x.v
	mut wg := sync.new_waitgroup()
	spawn fn [mut x, mut wg, c] () {
		wg.add(1)
		time.sleep(0)
		x.v = 1
		wg.done()
		c <- true
	}()
	spawn fn [mut x, mut wg, c] () {
		wg.add(1)
		time.sleep(0)
		x.v = 2
		wg.done()
		c <- true
	}()
	wg.wait()
	_ = <-c
	_ = <-c
}

fn test_race_wait_group_wrong_add() {
	c := chan bool{cap: 2}
	mut wg := sync.new_waitgroup()
	spawn fn [mut wg, c] () {
		wg.add(1)
		time.sleep(100 * time.millisecond)
		wg.done()
		c <- true
	}()
	spawn fn [mut wg, c] () {
		wg.add(1)
		time.sleep(100 * time.millisecond)
		wg.done()
		c <- true
	}()
	time.sleep(50 * time.millisecond)
	wg.wait()
	_ = <-c
	_ = <-c
}

fn test_no_race_wait_group_multiple_wait() {
	c := chan bool{cap: 2}
	mut wg := sync.new_waitgroup()
	spawn fn [mut wg, c] () {
		wg.wait()
		c <- true
	}()
	spawn fn [mut wg, c] () {
		wg.wait()
		c <- true
	}()
	wg.wait()
	_ = <-c
	_ = <-c
}

fn test_no_race_wait_group_multiple_wait2() {
	c := chan bool{cap: 2}
	mut wg := sync.new_waitgroup()
	wg.add(2)
	spawn fn [mut wg, c] () {
		wg.done()
		wg.wait()
		c <- true
	}()
	spawn fn [mut wg, c] () {
		wg.done()
		wg.wait()
		c <- true
	}()
	wg.wait()
	_ = <-c
	_ = <-c
}

fn test_no_race_wait_group_multiple_wait3() {
	mut data := &Cell[[3]int]{}
	done := chan bool{cap: p_count}
	mut wg := sync.new_waitgroup()
	wg.add(p_count)
	for p := 0; p < p_count; p++ {
		spawn fn [mut data, mut wg] (p int) {
			data.v[p] = 42
			wg.done()
		}(p)
	}
	for p := 0; p < p_count; p++ {
		spawn fn [mut data, mut wg, done] () {
			wg.wait()
			for p1 := 0; p1 < p_count; p1++ {
				_ = data.v[p1]
			}
			done <- true
		}()
	}
	for p := 0; p < p_count; p++ {
		_ = <-done
	}
}

// Correct usage but still a race
fn test_race_wait_group2() {
	mut x := &Cell[int]{}
	_ = x.v
	mut wg := sync.new_waitgroup()
	wg.add(2)
	spawn fn [mut x, mut wg] () {
		x.v = 1
		wg.done()
	}()
	spawn fn [mut x, mut wg] () {
		x.v = 2
		wg.done()
	}()
	wg.wait()
}

// V's WaitGroup panics with this message where Go's panics with
// "sync: negative WaitGroup counter".
const negative_wait_group_counter = 'Negative number of jobs in waitgroup'

fn test_no_race_wait_group_panic_recover() {
	mut x := 0
	mut wg := sync.new_waitgroup()
	defer {
		err := recover() or { 'no panic' }
		if err != negative_wait_group_counter {
			panic('Unexpected panic: ${err}')
		}
		x = 2
	}
	x = 1
	wg.add(-1)
}

fn test_no_race_wait_group_panic_recover2() {
	mut x := &Cell[int]{}
	_ = x.v
	mut wg := sync.new_waitgroup()
	ch := chan bool{cap: 1}
	f := fn [mut x, ch] () {
		x.v = 2
		ch <- true
	}
	spawn fn [mut x, mut wg, f] () {
		defer {
			_ = recover()
			spawn f()
		}
		x.v = 1
		wg.add(-1)
	}()
	_ = <-ch
}

fn test_no_race_wait_group_transitive() {
	mut x := &Cell[int]{}
	mut y := &Cell[int]{}
	mut wg := sync.new_waitgroup()
	wg.add(2)
	spawn fn [mut x, mut wg] () {
		x.v = 42
		wg.done()
	}()
	spawn fn [mut y, mut wg] () {
		time.sleep(10 * time.millisecond)
		y.v = 42
		wg.done()
	}()
	wg.wait()
	_ = x.v
	_ = y.v
}

fn test_no_race_wait_group_reuse() {
	mut data := &Cell[[3]int]{}
	mut wg := sync.new_waitgroup()
	for try := 0; try < 3; try++ {
		wg.add(p_count)
		for p := 0; p < p_count; p++ {
			spawn fn [mut data, mut wg] (p int) {
				data.v[p]++
				wg.done()
			}(p)
		}
		wg.wait()
		for p := 0; p < p_count; p++ {
			data.v[p]++
		}
	}
}

fn test_no_race_wait_group_reuse2() {
	mut data := &Cell[[3]int]{}
	mut wg := sync.new_waitgroup()
	for try := 0; try < 3; try++ {
		wg.add(p_count)
		for p := 0; p < p_count; p++ {
			spawn fn [mut data, mut wg] (p int) {
				data.v[p]++
				wg.done()
			}(p)
		}
		done := chan bool{}
		spawn fn [mut data, mut wg, done] () {
			wg.wait()
			for p := 0; p < p_count; p++ {
				data.v[p]++
			}
			done <- true
		}()
		wg.wait()
		_ = <-done
		for p := 0; p < p_count; p++ {
			data.v[p]++
		}
	}
}

fn test_race_wait_group_reuse() {
	done := chan bool{cap: t_count}
	mut wg := sync.new_waitgroup()
	for try := 0; try < t_count; try++ {
		mut data := &Cell[[3]int]{}
		wg.add(p_count)
		for p := 0; p < p_count; p++ {
			spawn fn [mut data, mut wg] (p int) {
				time.sleep(50 * time.millisecond)
				data.v[p]++
				wg.done()
			}(p)
		}
		spawn fn [mut data, mut wg, done] () {
			defer {
				// Unlike Go's GOMAXPROCS=1 harness, parallel V threads can detect this
				// intentional misuse before the waiter returns. Keep its race report
				// without terminating the remaining tests or leaving done blocked.
				if err := recover() {
					if err != 'WaitGroup misuse: reused before previous wait() returned' {
						panic('Unexpected panic: ${err}')
					}
				}
				done <- true
			}
			wg.wait()
			for p := 0; p < p_count; p++ {
				data.v[p]++
			}
		}()
		time.sleep(100 * time.millisecond)
		wg.wait()
	}
	for try := 0; try < t_count; try++ {
		_ = <-done
	}
}

fn test_no_race_wait_group_concurrent_add() {
	p_count4 := 4 // Go's `const P = 4`
	waiting := chan bool{cap: p_count4}
	mut wg := sync.new_waitgroup()
	for p := 0; p < p_count4; p++ {
		spawn fn [mut wg, waiting] () {
			wg.add(1)
			waiting <- true
			wg.done()
		}()
	}
	for p := 0; p < p_count4; p++ {
		_ = <-waiting
	}
	wg.wait()
}
