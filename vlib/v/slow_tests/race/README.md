# Go's race detector test suite, in V

`testdata/` is a V translation of the test suite of Go's race detector,
[src/runtime/race/testdata](https://github.com/golang/go/tree/master/src/runtime/race/testdata)
(golang/go@522ebc8370421796f13715117803d3302e8b61bd). `go_race_test.v` runs it the way Go's
`src/runtime/race/race_test.go` runs the Go tests:

* every program in `testdata/` is built with `v -race` and runs its tests one after another;
* a test whose name starts with `test_race_` passes when the race detector reported a data
  race while it ran, a `test_no_race_` test passes when it reported none.

```shell
v vlib/v/slow_tests/race/go_race_test.v
VRACE_GO_TEST_ONLY=chan,mop1 VRACE_GO_TEST_VERBOSE=1 v vlib/v/slow_tests/race/go_race_test.v
```

`VRACE_GO_TEST_ONLY` selects programs by name, `VRACE_GO_TEST_VERBOSE` prints the race
reports of the tests that failed. `VFLAGS='-cc gcc'` runs the suite with another C compiler.

## Translation

One V program per Go file (`mop_test.go`, the largest, is split into `mop1.v` .. `mop3.v`).
Go's `TestRaceIntRWClosures` is V's `test_race_int_rw_closures`, and `main()` runs the tests
in the order of the Go file, printing `=== RUN   <name>` before each one, like `go test -v`.

Each test keeps the shared variables, the reads and writes, and the synchronization of the Go
test. Go closures capture variables by reference, V closures by value, so the variables that
Go goroutines share are heap objects (`Cell[T]`) or globals in V; `go f()` is `spawn f()`.

## Differences from Go's harness

Go's harness runs the tests with `GOMAXPROCS=1`: some tests only race in the execution order
that gives (a goroutine runs once the test blocks), and ThreadSanitizer can miss a race whose
two accesses happen at the very same time. V threads run in parallel, so:

* when a race test missed its race, the program runs again, up to 3 times; a `test_race_`
  test passes when any run reported its race, a `test_no_race_` test fails when any run
  reported a race;
* a race report is attributed to the test in whose code its first program stack frame is,
  as the thread that reports a race can still run after its test returned;
* `run()` gives the threads of a test 20 ms to finish before the next test starts;
* `test_race_wait_group_reuse` recovers the expected WaitGroup reuse panic, which parallel
  V threads can detect despite the sleeps, so the intentional race can still be checked;
* `test_race_as_func3` sleeps in its thread, to get the order that it needs.

gcc's ThreadSanitizer instrumentation does not see the reads and writes of whole struct
values in call arguments and results (a struct passed by value, a struct result stored
through the return slot), which V uses for strings, arrays and maps. `v -race` uses clang when
it is installed, and the 20 tests that only pass with it count as known gcc limitations when
the suite runs with gcc.

## Not translated

26 of Go's 370 race tests need a Go feature that V does not have:

| Go file | Go tests | Reason |
| --- | --- | --- |
| mop_test.go | TestNoRaceIssue60934 | the race state of reused goroutines |
| mutex_test.go | TestNoRaceMutexSemaphore | unlocking a mutex on another thread |
| atomic_test.go | TestNoRaceAtomicCrash | recovering from a nil dereference (a signal) |
| sync_test.go | TestNoRaceNilMutexCrash | recovering from a nil dereference (a signal) |
| time_test.go | TestNoRaceAfterFuncReset, TestNoRaceTimerReset | `Timer.Reset` |
| time_test.go | TestNoRaceTicker, TestNoRaceTickerReset | `time.Ticker` |
| reflect_test.go | all 3 | runtime reflection |
| pool_test.go | all 2 | `sync.Pool` |
| finalizer_test.go | all 5 | finalizers and cleanups (race builds do not use a GC) |
| rangefunc_test.go | all 2 | range-over-func iterators (both skipped in Go too) |
| synctest_test.go | all 6 | `testing/synctest` bubbles |

V's `sync.Mutex` is a pthread mutex, so `test_no_race_mutex_example_from_html` hands off with a
`sync.Semaphore`, where Go unlocks the mutex in another goroutine.
