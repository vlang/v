@[has_globals]
module main

// Translated from Go's src/runtime/race/testdata/regression_test.go, see ../README.md.

// Code patterns that caused problems in the past.
// The functions that no test calls are only compiled, like in Go; `@[markused]` makes V
// generate them, as Go compiles every function of a package.

import time

struct Cell[T] {
mut:
	v T
}

fn main() {
	run('test_race_unaddressable_map_len', test_race_unaddressable_map_len)
	run('test_no_race_stack_push_pop', test_no_race_stack_push_pop)
	run('test_no_race_rpc_chan', test_no_race_rpc_chan)
	run('test_no_race_return', test_no_race_return)
	run('test_no_race_for_infinite_loop', test_no_race_for_infinite_loop)
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

struct LogImpl {
	x int
}

// new_log has Go's named result `l`, which the goroutine captures by reference.
fn new_log() LogImpl {
	mut l := &Cell[LogImpl]{}
	c := chan bool{}
	spawn fn [l, c] () {
		_ = l.v
		c <- true
	}()
	l.v = LogImpl{}
	_ = <-c
	return l.v
}

// Go's `var _ LogImpl = NewLog()`: a package level variable, initialized before the tests run.
__global global_log = new_log()

fn make_map() map[int]int {
	return map[int]int{}
}

@[markused]
fn instrument_map_len() {
	_ = make_map().len
}

@[markused]
fn instrument_map_len2() {
	m := map[int]map[int]int{}
	_ = m[0].len
}

@[markused]
fn instrument_map_len3() {
	m := map[int]&map[int]int{}
	_ = unsafe { (*m[0]).len }
}

fn test_race_unaddressable_map_len() {
	mut m := map[int]map[int]int{}
	ch := chan int{cap: 1}
	m[0] = map[int]int{}
	spawn fn (mut m map[int]map[int]int, ch chan int) {
		_ = m[0].len
		ch <- 0
	}(mut m, ch)
	m[0][0] = 1
	_ = <-ch
}

struct Rect {
	x int
	y int
}

struct Image {
	min Rect
	max Rect
}

@[noinline]
fn new_image() Image {
	return Image{}
}

@[markused]
fn addr_of_temp() {
	_ = new_image().min
}

type TypeID = int

@[markused]
fn (t &TypeID) encode_type(x int) !TypeID {
	match x {
		0 {
			return t.encode_type(x * x)!
		}
		else {}
	}
	return 0
}

type Stack = []int

fn (mut s Stack) push(x int) {
	s << x
}

fn (mut s Stack) pop() int {
	i := s.len
	n := s[i - 1]
	s = s[..i - 1]
	return n
}

fn test_no_race_stack_push_pop() {
	mut s := Stack([]int{})
	spawn fn (s &Stack) {}(&s)
	s.push(1)
	x := s.pop()
	_ = x
}

struct RpcChan {
	c chan bool
}

__global make_chan_calls int

@[noinline]
fn make_chan() &RpcChan {
	make_chan_calls++
	c := &RpcChan{chan bool{cap: 1}}
	c.c <- true
	return c
}

fn call() bool {
	x := <-make_chan().c
	return x
}

fn test_no_race_rpc_chan() {
	make_chan_calls = 0
	_ = call()
	if make_chan_calls != 1 {
		panic('make_chan_calls ${make_chan_calls}, expected 1\n')
	}
}

@[markused]
fn div_in_slice() {
	v := []i64{len: 10}
	i := 1
	_ = v[(i * 4) / 3]
}

fn test_no_race_return() {
	c := chan int{}
	no_race_return(c)
	_ = <-c
}

// Return used to do an implicit a = a, causing a read/write race
// with the goroutine. Compiler has an optimization to avoid that now.
// See issue 4014.
// Go's named result `a` is captured by reference by the goroutine.
fn no_race_return(c chan int) (int, int) {
	mut a := &Cell[int]{}
	a.v = 42
	spawn fn [a, c] () {
		_ = a.v
		c <- 1
	}()
	return a.v, 10
}

@[markused]
fn issue5431() {
	p := &&InlType(unsafe { nil })
	if inlinetest(p).x && inlinetest(p).y {
	} else if inlinetest(p).x || inlinetest(p).y {
	}
}

struct InlType {
	x bool
	y bool
}

fn inlinetest(p &&InlType) &InlType {
	return unsafe { *p }
}

interface Iface {
	foo() &FooResult
}

// FooResult is Go's anonymous `struct{ b bool }`.
struct FooResult {
	b bool
}

type Int = int

fn (i Int) foo() &FooResult {
	return &FooResult{false}
}

fn test_no_race_for_infinite_loop() {
	x := Int(0)
	// interface conversion causes nodes to be put on init list
	for Iface(x).foo().b {
	}
}
