@[has_globals]
module main

// Translated from Go's src/runtime/race/testdata/cgo_test.go and cgo_test_main.go, see
// ../README.md.
// Go's TestNoRaceCgoSync runs cgo_test_main.go with `go run -race`, because a Go test file
// cannot use cgo, and fails if that program fails (a race report makes it exit with status
// 66). V calls C directly, so here the body of that program's main() is the test.
// cgo_test_main.go defines Notify and Wait in C, in its cgo preamble; V cannot embed C source
// in a .v file, so they are V functions that call the same C builtin. Go's race detector
// does not see inside C code, and relies on its cgo calls acting as synchronization points;
// V compiles C code with ThreadSanitizer, which sees the synchronization done by the C
// builtin itself.

import time

fn C.__sync_fetch_and_add(ptr &i32, value i32) i32

fn main() {
	run('test_no_race_cgo_sync', test_no_race_cgo_sync)
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

struct Cell[T] {
mut:
	v T
}

// cgo_sync is the `int sync;` of the C code in cgo_test_main.go (a C int is a V i32).
__global cgo_sync i32

// notify is the C function `Notify` of cgo_test_main.go.
fn notify() {
	C.__sync_fetch_and_add(&cgo_sync, 1)
}

// wait is the C function `Wait` of cgo_test_main.go.
fn wait() {
	for C.__sync_fetch_and_add(&cgo_sync, 0) == 0 {
	}
}

// test_no_race_cgo_sync is the main() of cgo_test_main.go.
fn test_no_race_cgo_sync() {
	mut data := &Cell[int]{}
	spawn fn [mut data] () {
		data.v = 1
		notify()
	}()
	wait()
	_ = data.v
}
