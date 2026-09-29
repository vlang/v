// Translated from Go's src/runtime/race/testdata/mop_test.go (part 2: TestRaceIfaceWW .. before TestRaceFuncCall), see ../README.md.
import math.complex
import sync
import time

struct Cell[T] {
mut:
	v T
}

struct Point {
	x int
	y int
}

struct DummyWriter {
	state int
}

// Go's nil-able interface variables are V options of the interface type (nil -> none).
interface Writer {
	write(p []u8) int
}

fn (d DummyWriter) write(_ []u8) int {
	return 0
}

// Any is Go's empty interface `any`.
interface Any {}

struct OsFile {}

fn (_ &OsFile) read() {
}

interface IoReader {
	read()
}

// X is a type local to TestRaceStructInit in Go.
struct X {
	x int
	y int
}

interface Inter {
	foo(x int)
}

struct InterImpl {
	x int
	y int
}

@[noinline]
fn (p InterImpl) foo(_ int) {
}

// An empty struct, for Go's `chan struct{}`.
struct Empty {}

fn main() {
	run('test_race_iface_ww', test_race_iface_ww)
	run('test_race_iface_cmp', test_race_iface_cmp)
	run('test_race_iface_cmp_nil', test_race_iface_cmp_nil)
	run('test_race_eface_conv', test_race_eface_conv)
	run('test_race_iface_conv', test_race_iface_conv)
	run('test_race_error', test_race_error)
	run('test_race_intptr_rw', test_race_intptr_rw)
	run('test_race_string_rw', test_race_string_rw)
	run('test_race_string_ptr_rw', test_race_string_ptr_rw)
	run('test_race_float64_ww', test_race_float64_ww)
	run('test_race_complex128_ww', test_race_complex128_ww)
	run('test_race_unsafe_ptr_rw', test_race_unsafe_ptr_rw)
	run('test_race_func_variable_rw', test_race_func_variable_rw)
	run('test_race_func_variable_ww', test_race_func_variable_ww)
	run('test_race_panic', test_race_panic)
	run('test_no_race_blank', test_no_race_blank)
	run('test_race_append_rw', test_race_append_rw)
	run('test_race_append_len_rw', test_race_append_len_rw)
	run('test_race_append_cap_rw', test_race_append_cap_rw)
	run('test_no_race_func_args_rw', test_no_race_func_args_rw)
	run('test_race_func_args_rw', test_race_func_args_rw)
	run('test_race_crawl', test_race_crawl)
	run('test_race_indirection', test_race_indirection)
	run('test_race_rune', test_race_rune)
	run('test_race_empty_interface1', test_race_empty_interface1)
	run('test_race_empty_interface2', test_race_empty_interface2)
	run('test_race_tls', test_race_tls)
	run('test_no_race_heap_reallocation', test_no_race_heap_reallocation)
	run('test_race_and', test_race_and)
	run('test_race_and2', test_race_and2)
	run('test_no_race_and', test_no_race_and)
	run('test_race_or', test_race_or)
	run('test_race_or2', test_race_or2)
	run('test_no_race_or', test_no_race_or)
	run('test_no_race_short_calc', test_no_race_short_calc)
	run('test_no_race_short_calc2', test_no_race_short_calc2)
	run('test_race_func_itself', test_race_func_itself)
	run('test_no_race_func_unlock', test_no_race_func_unlock)
	run('test_race_struct_init', test_race_struct_init)
	run('test_race_array_init', test_race_array_init)
	run('test_race_map_init', test_race_map_init)
	run('test_race_map_init2', test_race_map_init2)
	run('test_race_inter_call', test_race_inter_call)
	run('test_race_inter_call2', test_race_inter_call2)
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

fn test_race_iface_ww() {
	mut a := &Cell[?Writer]{}
	mut b := ?Writer(none)
	ch := chan bool{cap: 1}
	spawn fn [mut a, ch] () {
		a.v = DummyWriter{1}
		ch <- true
	}()
	a.v = DummyWriter{2}
	_ = <-ch
	b = a.v
	a.v = b
}

fn test_race_iface_cmp() {
	mut a := &Cell[?Writer]{}
	b := ?Writer(none)
	a.v = DummyWriter{1}
	ch := chan bool{cap: 1}
	spawn fn [mut a, ch] () {
		a.v = DummyWriter{1}
		ch <- true
	}()
	_ = a.v == b
	_ = <-ch
}

fn test_race_iface_cmp_nil() {
	mut a := &Cell[?Writer]{}
	a.v = DummyWriter{1}
	ch := chan bool{cap: 1}
	spawn fn [mut a, ch] () {
		a.v = DummyWriter{1}
		ch <- true
	}()
	_ = a.v == none
	_ = <-ch
}

fn test_race_eface_conv() {
	c := chan bool{}
	mut v := &Cell[int]{}
	spawn fn [v, c] () {
		spawn fn (_ Any) {}(v.v)
		c <- true
	}()
	v.v = 42
	_ = <-c
}

fn test_race_iface_conv() {
	c := chan bool{}
	mut f := &Cell[&OsFile]{
		v: &OsFile{}
	}
	spawn fn [f, c] () {
		spawn fn (_ IoReader) {}(f.v)
		c <- true
	}()
	f.v = &OsFile{}
	_ = <-c
}

fn test_race_error() {
	ch := chan bool{cap: 1}
	mut err := &Cell[?IError]{}
	spawn fn [mut err, ch] () {
		err.v = none
		ch <- true
	}()
	_ = err.v
	_ = <-ch
}

fn test_race_intptr_rw() {
	mut x := &Cell[int]{}
	mut y := 0
	p := &x.v
	ch := chan bool{cap: 1}
	spawn fn [p, ch] () {
		unsafe {
			*p = 5
		}
		ch <- true
	}()
	y = *p
	x.v = y
	_ = <-ch
}

fn test_race_string_rw() {
	ch := chan bool{cap: 1}
	mut s := &Cell[string]{}
	spawn fn [mut s, ch] () {
		s.v = 'abacaba'
		ch <- true
	}()
	_ = s.v
	_ = <-ch
}

fn test_race_string_ptr_rw() {
	ch := chan bool{cap: 1}
	mut x := &Cell[string]{}
	p := &x.v
	spawn fn [p, ch] () {
		unsafe {
			*p = 'a'
		}
		ch <- true
	}()
	_ = *p
	_ = <-ch
}

fn test_race_float64_ww() {
	mut x := &Cell[f64]{}
	mut y := 0.0
	ch := chan bool{cap: 1}
	spawn fn [mut x, ch] () {
		x.v = 1.0
		ch <- true
	}()
	x.v = 2.0
	_ = <-ch

	y = x.v
	x.v = y
}

// Go's complex128 is V's math.complex.Complex (two f64 fields, 16 bytes like complex128).
fn test_race_complex128_ww() {
	mut x := &Cell[complex.Complex]{}
	mut y := complex.Complex{}
	ch := chan bool{cap: 1}
	spawn fn [mut x, ch] () {
		x.v = complex.complex(2, 2)
		ch <- true
	}()
	x.v = complex.complex(4, 4)
	_ = <-ch

	y = x.v
	x.v = y
}

fn test_race_unsafe_ptr_rw() {
	mut x := &Cell[int]{}
	mut y := 0
	mut z := &Cell[int]{}
	x.v, y, z.v = 1, 2, 3
	mut p := &Cell[voidptr]{
		v: voidptr(&x.v)
	}
	ch := chan bool{cap: 1}
	spawn fn [mut p, z, ch] () {
		p.v = voidptr(&z.v)
		ch <- true
	}()
	y = unsafe { *(&int(p.v)) }
	x.v = y
	_ = <-ch
}

fn test_race_func_variable_rw() {
	mut f := &Cell[fn (int) int]{}
	f.v = fn (x int) int {
		return x * x
	}
	ch := chan bool{cap: 1}
	spawn fn [mut f, ch] () {
		f.v = fn (x int) int {
			return x
		}
		ch <- true
	}()
	mut y := f.v(1)
	_ = <-ch
	x := y
	y = x
}

fn test_race_func_variable_ww() {
	mut f := &Cell[fn (int) int]{}
	ch := chan bool{cap: 1}
	spawn fn [mut f, ch] () {
		f.v = fn (x int) int {
			return x
		}
		ch <- true
	}()
	f.v = fn (x int) int {
		return x * x
	}
	_ = <-ch
}

fn test_race_panic() {
	mut x := &Cell[int]{}
	_ = x.v
	mut zero := &Cell[int]{}
	ch := chan bool{cap: 2}
	spawn fn [mut x, mut zero, ch] () {
		defer {
			_ = recover() or { panic('should be panicking') }
			x.v = 1
			ch <- true
		}
		y := 1 / zero.v
		zero.v = y
	}()
	spawn fn [mut x, mut zero, ch] () {
		defer {
			_ = recover() or { panic('should be panicking') }
			x.v = 2
			ch <- true
		}
		y := 1 / zero.v
		zero.v = y
	}()
	_ = <-ch
	_ = <-ch
	if zero.v != 0 {
		panic('zero has changed')
	}
}

fn test_no_race_blank() {
	mut a := &Cell[[5]int]{}
	ch := chan bool{cap: 1}
	spawn fn [a, ch] () {
		_, _ = a.v[0], a.v[1]
		ch <- true
	}()
	_, _ = a.v[2], a.v[3]
	_ = <-ch
	a.v[1] = a.v[0]
}

fn test_race_append_rw() {
	mut a := []int{len: 10}
	ch := chan bool{}
	// Go's `_ = append(a, 1)`: `a` is a copy of the header with len == cap, so the append
	// allocates a new buffer and copies (reads) the elements of the shared one.
	spawn fn [mut a, ch] () {
		a << 1
		ch <- true
	}()
	a[0] = 1
	_ = <-ch
}

fn test_race_append_len_rw() {
	mut a := &Cell[[]int]{
		v: []int{len: 0}
	}
	ch := chan bool{}
	spawn fn [mut a, ch] () {
		a.v << 1
		ch <- true
	}()
	_ = a.v.len
	_ = <-ch
}

fn test_race_append_cap_rw() {
	mut a := &Cell[[]int]{
		v: []int{len: 0}
	}
	ch := chan string{}
	spawn fn [mut a, ch] () {
		a.v << 1
		ch <- ''
	}()
	_ = a.v.cap
	_ = <-ch
}

fn test_no_race_func_args_rw() {
	ch := chan u8{cap: 1}
	mut x := &Cell[u8]{}
	spawn fn [ch] (y u8) {
		_ = y
		ch <- 0
	}(x.v)
	x.v = 1
	_ = <-ch
}

fn test_race_func_args_rw() {
	ch := chan u8{cap: 1}
	mut x := &Cell[u8]{}
	spawn fn [ch] (y &u8) {
		_ = *y
		ch <- 0
	}(&x.v)
	x.v = 1
	_ = <-ch
}

// from the mailing list, slightly modified
// unprotected concurrent access to seen[]
fn test_race_crawl() {
	url := 'dummyurl'
	depth := 3
	mut seen := &Cell[map[string]bool]{}
	ch := chan int{cap: 100}
	mut wg := sync.new_waitgroup()
	mut crawl := &Cell[fn (string, int)]{}
	crawl.v = fn [mut seen, ch, mut wg, crawl] (u string, d int) {
		mut nurl := 0
		defer {
			ch <- nurl
		}
		seen.v[u] = true
		if d <= 0 {
			wg.done()
			return
		}
		urls := ['a', 'b', 'c']!
		for uu in urls {
			if uu !in seen.v {
				wg.add(1)
				spawn crawl.v(uu, d - 1)
				nurl++
			}
		}
		wg.done()
	}
	wg.add(1)
	spawn crawl.v(url, depth)
	wg.wait()
}

fn test_race_indirection() {
	ch := chan Empty{cap: 1}
	mut y := &Cell[int]{}
	x := &y.v
	spawn fn [x, ch] () {
		unsafe {
			*x = 1
		}
		ch <- Empty{}
	}()
	unsafe {
		*x = 2
	}
	_ = <-ch
	_ = *x
}

fn test_race_rune() {
	c := chan bool{}
	mut x := &Cell[rune]{}
	spawn fn [mut x, c] () {
		x.v = 1
		c <- true
	}()
	_ = x.v
	_ = <-c
}

fn test_race_empty_interface1() {
	c := chan bool{}
	mut x := &Cell[?Any]{}
	spawn fn [mut x, c] () {
		x.v = none
		c <- true
	}()
	_ = x.v
	_ = <-c
}

fn test_race_empty_interface2() {
	c := chan bool{}
	mut x := &Cell[?Any]{}
	spawn fn [mut x, c] () {
		x.v = &Point{}
		c <- true
	}()
	_ = x.v
	_ = <-c
}

fn test_race_tls() {
	comm := chan &int{}
	done := chan bool{cap: 2}
	spawn fn [comm, done] () {
		mut x := 0
		comm <- &x
		x = 1
		x = *(<-comm)
		done <- true
	}()
	spawn fn [comm, done] () {
		p := <-comm
		unsafe {
			*p = 2
		}
		comm <- p
		done <- true
	}()
	_ = <-done
	_ = <-done
}

fn test_no_race_heap_reallocation() {
	// It is possible that a future implementation
	// of memory allocation will ruin this test.
	// Increasing n might help in this case, so
	// this test is a bit more generic than most of the
	// others.
	n := 2
	done := chan bool{cap: n}
	empty := fn (p &int) {
		_ = p
	}
	for i := 0; i < n; i++ {
		ms := i
		spawn fn [ms, empty, done] () {
			time.sleep(ms * time.millisecond)
			x := &Cell[int]{}
			empty(&x.v) // x goes to the heap
			done <- true
		}()
	}
	for i := 0; i < n; i++ {
		_ = <-done
	}
}

fn test_race_and() {
	c := chan bool{}
	mut x := &Cell[int]{}
	y := 0
	spawn fn [mut x, c] () {
		x.v = 1
		c <- true
	}()
	if x.v == 1 && y == 1 {
	}
	_ = <-c
}

fn test_race_and2() {
	c := chan bool{}
	mut x := &Cell[int]{}
	y := 0
	spawn fn [mut x, c] () {
		x.v = 1
		c <- true
	}()
	if y == 0 && x.v == 1 {
	}
	_ = <-c
}

fn test_no_race_and() {
	c := chan bool{}
	mut x := &Cell[int]{}
	y := 0
	spawn fn [mut x, c] () {
		x.v = 1
		c <- true
	}()
	if y == 1 && x.v == 1 {
	}
	_ = <-c
}

fn test_race_or() {
	c := chan bool{}
	mut x := &Cell[int]{}
	y := 0
	spawn fn [mut x, c] () {
		x.v = 1
		c <- true
	}()
	if x.v == 1 || y == 1 {
	}
	_ = <-c
}

fn test_race_or2() {
	c := chan bool{}
	mut x := &Cell[int]{}
	y := 0
	spawn fn [mut x, c] () {
		x.v = 1
		c <- true
	}()
	if y == 1 || x.v == 1 {
	}
	_ = <-c
}

fn test_no_race_or() {
	c := chan bool{}
	mut x := &Cell[int]{}
	y := 0
	spawn fn [mut x, c] () {
		x.v = 1
		c <- true
	}()
	if y == 0 || x.v == 1 {
	}
	_ = <-c
}

fn test_no_race_short_calc() {
	c := chan bool{}
	x := 0
	mut y := &Cell[int]{}
	spawn fn [mut y, c] () {
		y.v = 1
		c <- true
	}()
	if x == 0 || y.v == 0 {
	}
	_ = <-c
}

fn test_no_race_short_calc2() {
	c := chan bool{}
	x := 0
	mut y := &Cell[int]{}
	spawn fn [mut y, c] () {
		y.v = 1
		c <- true
	}()
	if x == 1 && y.v == 0 {
	}
	_ = <-c
}

fn test_race_func_itself() {
	c := chan bool{}
	mut f := &Cell[fn ()]{
		v: fn () {}
	}
	spawn fn [f, c] () {
		f.v()
		c <- true
	}()
	f.v = fn () {}
	_ = <-c
}

fn test_no_race_func_unlock() {
	ch := chan bool{cap: 1}
	mut mu := sync.new_mutex()
	mut x := &Cell[int]{}
	spawn fn [mut mu, mut x, ch] () {
		mu.lock()
		x.v = 42
		mu.unlock()
		ch <- true
	}()
	x.v = fn (mut mu sync.Mutex) int {
		mu.lock()
		return 43
	}(mut mu)
	mu.unlock()
	_ = <-ch
}

fn test_race_struct_init() {
	c := chan bool{cap: 1}
	mut y := &Cell[int]{}
	spawn fn [mut y, c] () {
		y.v = 42
		c <- true
	}()
	x := X{
		x: y.v
	}
	_ = x
	_ = <-c
}

fn test_race_array_init() {
	c := chan bool{cap: 1}
	mut y := &Cell[int]{}
	spawn fn [mut y, c] () {
		y.v = 42
		c <- true
	}()
	x := [0, y.v, 42]
	_ = x
	_ = <-c
}

fn test_race_map_init() {
	c := chan bool{cap: 1}
	mut y := &Cell[int]{}
	spawn fn [mut y, c] () {
		y.v = 42
		c <- true
	}()
	x := {
		0:   42
		y.v: 42
	}
	_ = x
	_ = <-c
}

fn test_race_map_init2() {
	c := chan bool{cap: 1}
	mut y := &Cell[int]{}
	spawn fn [mut y, c] () {
		y.v = 42
		c <- true
	}()
	x := {
		0:  42
		42: y.v
	}
	_ = x
	_ = <-c
}

fn test_race_inter_call() {
	c := chan bool{cap: 1}
	p := InterImpl{}
	mut x := &Cell[Inter]{
		v: p
	}
	spawn fn [mut x, c] () {
		p2 := InterImpl{}
		x.v = p2
		c <- true
	}()
	x.v.foo(0)
	_ = <-c
}

fn test_race_inter_call2() {
	c := chan bool{cap: 1}
	p := InterImpl{}
	x := Inter(p)
	mut z := &Cell[int]{}
	spawn fn [mut z, c] () {
		z.v = 42
		c <- true
	}()
	x.foo(z.v)
	_ = <-c
}
