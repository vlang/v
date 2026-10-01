// Translated from Go's src/runtime/race/testdata/select_test.go, see ../README.md.
import time

struct Cell[T] {
mut:
	v T
}

// Task is declared inside Go's TestNoRaceSelect4; V has no local types.
struct Task {
	f    fn () = unsafe { nil }
	done chan bool
}

fn main() {
	run('test_no_race_select1', test_no_race_select1)
	run('test_no_race_select2', test_no_race_select2)
	run('test_no_race_select3', test_no_race_select3)
	run('test_no_race_select4', test_no_race_select4)
	run('test_no_race_select5', test_no_race_select5)
	run('test_race_select1', test_race_select1)
	run('test_race_select2', test_race_select2)
	run('test_race_select3', test_race_select3)
	run('test_race_select4', test_race_select4)
	run('test_race_select5', test_race_select5)
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

fn test_no_race_select1() {
	mut x := &Cell[int]{}
	_ = x.v
	compl := chan bool{}
	c := chan bool{}
	c1 := chan bool{}
	spawn fn [mut x, c, c1, compl] () {
		x.v = 1
		// At least two channels are needed because
		// otherwise the compiler optimizes select out.
		// See comment in runtime/select.go:^func selectgo.
		select {
			c <- true {
			}
			c1 <- true {
			}
		}
		compl <- true
	}()
	select {
		_ := <-c {
		}
		c1 <- true {
		}
	}
	x.v = 2
	_ = <-compl
}

fn test_no_race_select2() {
	mut x := &Cell[int]{}
	_ = x.v
	compl := chan bool{}
	c := chan bool{}
	c1 := chan bool{}
	spawn fn [mut x, c, c1, compl] () {
		select {
			_ := <-c {
			}
			_ := <-c1 {
			}
		}
		x.v = 1
		compl <- true
	}()
	x.v = 2
	c.close()
	time.sleep(0)
	_ = <-compl
}

fn test_no_race_select3() {
	mut x := &Cell[int]{}
	_ = x.v
	compl := chan bool{}
	c := chan bool{cap: 10}
	c1 := chan bool{}
	spawn fn [mut x, c, c1, compl] () {
		x.v = 1
		select {
			c <- true {
			}
			_ := <-c1 {
			}
		}
		compl <- true
	}()
	_ = <-c
	x.v = 2
	_ = <-compl
}

fn test_no_race_select4() {
	queue := chan Task{}
	dummy := chan bool{}

	spawn fn [queue] () {
		for {
			select {
				t := <-queue {
					t.f()
					t.done <- true
				}
			}
		}
	}()

	doit := fn [queue, dummy] (f fn ()) {
		done := chan bool{cap: 1}
		task := Task{f, done}
		select {
			queue <- task {
			}
			_ := <-dummy {
			}
		}
		select {
			_ := <-done {
			}
			_ := <-dummy {
			}
		}
	}

	mut x := &Cell[int]{}
	doit(fn [mut x] () {
		x.v = 1
	})
	_ = x.v
}

fn test_no_race_select5() {
	test := fn (sel bool, need_sched bool) {
		mut x := &Cell[int]{}
		_ = x.v
		ch := chan bool{}
		c1 := chan bool{}

		done := chan bool{cap: 2}
		spawn fn [need_sched, mut x, sel, ch, c1, done] () {
			if need_sched {
				time.sleep(0)
			}
			// println(1)
			x.v = 1
			if sel {
				select {
					ch <- true {
					}
					_ := <-c1 {
					}
				}
			} else {
				ch <- true
			}
			done <- true
		}()

		spawn fn [sel, ch, c1, mut x, done] () {
			// println(2)
			if sel {
				select {
					_ := <-ch {
					}
					_ := <-c1 {
					}
				}
			} else {
				_ = <-ch
			}
			x.v = 1
			done <- true
		}()
		_ = <-done
		_ = <-done
	}

	test(true, true)
	test(true, false)
	test(false, true)
	test(false, false)
}

fn test_race_select1() {
	mut x := &Cell[int]{}
	_ = x.v
	compl := chan bool{cap: 2}
	c := chan bool{}
	c1 := chan bool{}

	spawn fn [c] () {
		_ = <-c
		_ = <-c
	}()
	f := fn [c, c1, mut x, compl] () {
		select {
			c <- true {
			}
			c1 <- true {
			}
		}
		x.v = 1
		compl <- true
	}
	spawn f()
	spawn f()
	_ = <-compl
	_ = <-compl
}

fn test_race_select2() {
	mut x := &Cell[int]{}
	_ = x.v
	compl := chan bool{}
	c := chan bool{}
	c1 := chan bool{}
	spawn fn [mut x, c, c1, compl] () {
		x.v = 1
		select {
			_ := <-c {
			}
			_ := <-c1 {
			}
		}
		compl <- true
	}()
	c.close()
	x.v = 2
	_ = <-compl
}

fn test_race_select3() {
	mut x := &Cell[int]{}
	_ = x.v
	compl := chan bool{}
	c := chan bool{}
	c1 := chan bool{}
	spawn fn [mut x, c, c1, compl] () {
		x.v = 1
		select {
			c <- true {
			}
			c1 <- true {
			}
		}
		compl <- true
	}()
	x.v = 2
	select {
		_ := <-c {
		}
	}
	_ = <-compl
}

fn test_race_select4() {
	done := chan bool{cap: 1}
	mut x := &Cell[int]{}
	spawn fn [mut x, done] () {
		select {
			else {
				x.v = 2
			}
		}
		done <- true
	}()
	_ = x.v
	_ = <-done
}

// The idea behind this test:
// there are two variables, access to one
// of them is synchronized, access to the other
// is not.
// Select must (unconditionally) choose the non-synchronized variable
// thus causing exactly one race.
// Currently this test doesn't look like it accomplishes
// this goal.
fn test_race_select5() {
	done := chan bool{cap: 1}
	c1 := chan bool{cap: 1}
	c2 := chan bool{}
	mut x := &Cell[int]{}
	mut y := &Cell[int]{}
	spawn fn [c1, mut x, c2, mut y, done] () {
		select {
			c1 <- true {
				x.v = 1
			}
			c2 <- true {
				y.v = 1
			}
		}
		done <- true
	}()
	_ = x.v
	_ = y.v
	_ = <-done
}

// select statements may introduce
// flakiness: whether this test contains
// a race depends on the scheduling
// (some may argue that the code contains
// this race by definition)
// (Go's TestFlakyDefault is commented out in select_test.go, so it is not translated.)
