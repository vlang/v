// Translated from Go's src/runtime/race/testdata/mop_test.go (part 1: TestRaceIntRWGlobalFuncs .. before TestRaceIfaceWW), see ../README.md.
@[has_globals]
module main

import time

struct Cell[T] {
mut:
	v T
}

struct Point {
mut:
	x int
	y int
}

struct NamedPoint {
mut:
	name string
	p    Point
}

struct DummyWriter {
mut:
	state int
}

interface Writer {
	write(p []u8) int
}

fn (d DummyWriter) write(p []u8) int {
	return 0
}

// Any is Go's `any`, the empty interface (V's own `any` is deprecated).
interface Any {}

__global global_x int
__global global_y int
__global global_ch = chan int{cap: 2}

fn global_func1() {
	global_y = global_x
	global_ch <- 1
}

fn global_func2() {
	global_x = 1
	global_ch <- 1
}

fn main() {
	run('test_race_int_rw_global_funcs', test_race_int_rw_global_funcs)
	run('test_race_int_rw_closures', test_race_int_rw_closures)
	run('test_no_race_int_rw_closures', test_no_race_int_rw_closures)
	run('test_race_int32_rw_closures', test_race_int32_rw_closures)
	run('test_no_race_case', test_no_race_case)
	run('test_race_case_condition', test_race_case_condition)
	run('test_race_case_condition2', test_race_case_condition2)
	run('test_race_case_body', test_race_case_body)
	run('test_no_race_case_fallthrough', test_no_race_case_fallthrough)
	run('test_race_case_fallthrough', test_race_case_fallthrough)
	run('test_race_case_issue6418', test_race_case_issue6418)
	run('test_race_case_type', test_race_case_type)
	run('test_race_case_type_body', test_race_case_type_body)
	run('test_race_case_type_issue5890', test_race_case_type_issue5890)
	run('test_no_race_range', test_no_race_range)
	run('test_no_race_range_issue5446', test_no_race_range_issue5446)
	run('test_race_range', test_race_range)
	run('test_race_for_init', test_race_for_init)
	run('test_no_race_for_init', test_no_race_for_init)
	run('test_race_for_test', test_race_for_test)
	run('test_race_for_incr', test_race_for_incr)
	run('test_no_race_for_incr', test_no_race_for_incr)
	run('test_race_plus', test_race_plus)
	run('test_race_plus2', test_race_plus2)
	run('test_no_race_plus', test_no_race_plus)
	run('test_race_complement', test_race_complement)
	run('test_race_div', test_race_div)
	run('test_race_div_const', test_race_div_const)
	run('test_race_mod', test_race_mod)
	run('test_race_mod_const', test_race_mod_const)
	run('test_race_rotate', test_race_rotate)
	run('test_no_race_enough_registers', test_no_race_enough_registers)
	run('test_race_func_argument', test_race_func_argument)
	run('test_race_func_argument2', test_race_func_argument2)
	run('test_race_sprint', test_race_sprint)
	run('test_race_array_copy', test_race_array_copy)
	run('test_race_nested_array_copy', test_race_nested_array_copy)
	run('test_race_struct_rw', test_race_struct_rw)
	run('test_race_struct_field_rw1', test_race_struct_field_rw1)
	run('test_no_race_struct_field_rw1', test_no_race_struct_field_rw1)
	run('test_no_race_struct_field_rw2', test_no_race_struct_field_rw2)
	run('test_race_struct_field_rw2', test_race_struct_field_rw2)
	run('test_race_struct_field_rw3', test_race_struct_field_rw3)
	run('test_race_eface_ww', test_race_eface_ww)
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

fn test_race_int_rw_global_funcs() {
	spawn global_func1()
	spawn global_func2()
	_ = <-global_ch
	_ = <-global_ch
}

fn test_race_int_rw_closures() {
	mut x := &Cell[int]{}
	mut y := &Cell[int]{}
	_ = y.v
	ch := chan int{cap: 2}

	spawn fn [x, mut y, ch] () {
		y.v = x.v
		ch <- 1
	}()
	spawn fn [mut x, ch] () {
		x.v = 1
		ch <- 1
	}()
	_ = <-ch
	_ = <-ch
}

fn test_no_race_int_rw_closures() {
	mut x := &Cell[int]{}
	mut y := &Cell[int]{}
	_ = y.v
	ch := chan int{cap: 1}

	spawn fn [x, mut y, ch] () {
		y.v = x.v
		ch <- 1
	}()
	_ = <-ch
	spawn fn [mut x, ch] () {
		x.v = 1
		ch <- 1
	}()
	_ = <-ch
}

fn test_race_int32_rw_closures() {
	mut x := &Cell[i32]{}
	mut y := &Cell[i32]{}
	_ = y.v
	ch := chan bool{cap: 2}

	spawn fn [x, mut y, ch] () {
		y.v = x.v
		ch <- true
	}()
	spawn fn [mut x, ch] () {
		x.v = 1
		ch <- true
	}()
	_ = <-ch
	_ = <-ch
}

fn test_no_race_case() {
	mut y := 0
	for x := -1; x <= 1; x++ {
		match true {
			x < 0 {
				y = -1
			}
			x == 0 {
				y = 0
			}
			x > 0 {
				y = 1
			}
			else {}
		}
	}
	y++
}

fn test_race_case_condition() {
	mut x := &Cell[int]{
		v: 0
	}
	ch := chan int{cap: 2}

	spawn fn [mut x, ch] () {
		x.v = 2
		ch <- 1
	}()
	spawn fn [mut x, ch] () {
		match x.v < 2 {
			true {
				x.v = 1
			}
			// false {
			//	x.v = 5
			// }
			else {}
		}
		ch <- 1
	}()
	_ = <-ch
	_ = <-ch
}

fn test_race_case_condition2() {
	// switch body is rearranged by the compiler so the tests
	// passes even if we don't instrument '<'
	mut x := &Cell[int]{
		v: 0
	}
	ch := chan int{cap: 2}

	spawn fn [mut x, ch] () {
		x.v = 2
		ch <- 1
	}()
	spawn fn [mut x, ch] () {
		match x.v < 2 {
			true {
				x.v = 1
			}
			false {
				x.v = 5
			}
		}
		ch <- 1
	}()
	_ = <-ch
	_ = <-ch
}

fn test_race_case_body() {
	mut x := &Cell[int]{}
	mut y := &Cell[int]{}
	_ = y.v
	ch := chan int{cap: 2}

	spawn fn [x, mut y, ch] () {
		y.v = x.v
		ch <- 1
	}()
	spawn fn [mut x, ch] () {
		// Go's `default:` comes first, but is only taken when no case matches, like V's `else`.
		match true {
			x.v == 100 {
				x.v = -x.v
			}
			else {
				x.v = 1
			}
		}
		ch <- 1
	}()
	_ = <-ch
	_ = <-ch
}

fn test_no_race_case_fallthrough() {
	mut x := &Cell[int]{}
	mut y := &Cell[int]{}
	mut z := &Cell[int]{}
	_ = y.v
	ch := chan int{cap: 2}
	z.v = 1

	spawn fn [x, mut y, ch] () {
		y.v = x.v
		ch <- 1
	}()
	spawn fn [mut x, z, ch] () {
		match true {
			z.v == 1 {}
			z.v == 2 {
				x.v = 2
			}
			else {}
		}
		ch <- 1
	}()
	_ = <-ch
	_ = <-ch
}

fn test_race_case_fallthrough() {
	mut x := &Cell[int]{}
	mut y := &Cell[int]{}
	mut z := &Cell[int]{}
	_ = y.v
	ch := chan int{cap: 2}
	z.v = 1

	spawn fn [x, mut y, ch] () {
		y.v = x.v
		ch <- 1
	}()
	spawn fn [mut x, z, ch] () {
		// V has no `fallthrough`: Go's `case z == 1: fallthrough` enters the body of
		// `case z == 2` without evaluating `z == 2`, like this branch with two conditions.
		match true {
			z.v == 1, z.v == 2 {
				x.v = 2
			}
			else {}
		}
		ch <- 1
	}()

	_ = <-ch
	_ = <-ch
}

fn test_race_case_issue6418() {
	mut m := {
		'a': {
			'b': 'c'
		}
	}
	ch := chan int{}
	spawn fn (mut m map[string]map[string]string, ch chan int) {
		m['a']['x'] = 'y'
		ch <- 1
	}(mut m, ch)
	// Go's `switch m["a"]["b"] {}` only evaluates the tag; V does not allow a `match`
	// without branches.
	_ = m['a']['b']
	_ = <-ch
}

fn test_race_case_type() {
	x := 0
	y := 0
	mut i := &Cell[Any]{
		v: x
	}
	c := chan int{cap: 1}
	spawn fn [i, c] () {
		// Go's `case nil:` has no V equivalent (a V interface is never nil), `else` takes
		// its place.
		match i.v {
			int {}
			else {}
		}
		c <- 1
	}()
	i.v = y
	_ = <-c
}

fn test_race_case_type_body() {
	mut x := &Cell[int]{}
	y := &Cell[int]{}
	i := Any(&x.v)
	c := chan int{cap: 1}
	spawn fn [i, y, c] () {
		// Go: `switch i := i.(type) { case nil: case *int: *i = y }`; V's `match` cannot
		// have a `&int` branch, so the type switch is an `if ... is`.
		if i is &int {
			unsafe {
				*i = y.v
			}
		}
		c <- 1
	}()
	x.v = y.v
	_ = <-c
}

fn test_race_case_type_issue5890() {
	// spurious extra instrumentation of the initial interface
	// value.
	x := 0
	y := 0
	mut m := map[int]map[int]Any{}
	m[0] = map[int]Any{}
	c := chan int{cap: 1}
	spawn fn [x, c] (mut m map[int]map[int]Any) {
		// Go: `switch i := m[0][1].(type) { case nil: case *int: *i = x }`, see
		// test_race_case_type_body.
		i := m[0][1]
		if i is &int {
			unsafe {
				*i = x
			}
		}
		c <- 1
	}(mut m)
	m[0][1] = y
	_ = <-c
}

fn test_no_race_range() {
	ch := chan int{cap: 3}
	a := [1, 2, 3]!
	for v in a {
		ch <- v
	}
	ch.close()
}

fn test_no_race_range_issue5446() {
	ch := chan int{cap: 3}
	mut a := [1, 2, 3]
	b := [4]
	// used to insert a spurious instrumentation of a[i]
	// and crash.
	mut i := 1
	// Go: `for i, a[i] = range b`; V's `for` always declares new loop variables.
	for idx, val in b {
		a[i] = val
		i = idx
		ch <- i
	}
	ch.close()
}

const race_range_n = 2

fn test_race_range() {
	a := [race_range_n]int{}
	mut x := &Cell[int]{}
	mut y := &Cell[int]{}
	_ = x.v + y.v
	done := chan bool{cap: race_range_n}
	// declare here (not in for stmt) so that i and v are shared w/ or w/o loop variable sharing change
	mut i := 0
	mut v := &Cell[int]{}
	for idx, val in a {
		i = idx
		v.v = val
		spawn fn [mut x, mut y, v, done] (i int) {
			// we don't want a write-vs-write race
			// so there is no array b here
			if i == 0 {
				x.v = v.v
			} else {
				y.v = v.v
			}
			done <- true
		}(i)
		// Ensure the goroutine runs before we continue the loop.
		time.sleep(0)
	}
	for _ in 0 .. race_range_n {
		_ = <-done
	}
}

fn test_race_for_init() {
	c := chan int{}
	mut x := &Cell[int]{}
	spawn fn [x, c] () {
		c <- x.v
	}()
	for x.v = 42; false; {
	}
	_ = <-c
}

fn test_no_race_for_init() {
	done := chan bool{}
	c := chan bool{}
	mut x := &Cell[int]{}
	spawn fn [mut x, c, done] () {
		for {
			_ = <-c or {
				done <- true
				return
			}
			x.v++
		}
	}()
	mut i := 0
	for x.v = 42; i < 10; i++ {
		c <- true
	}
	c.close()
	_ = <-done
}

fn test_race_for_test() {
	done := chan bool{}
	c := chan bool{}
	mut stop := &Cell[bool]{}
	spawn fn [mut stop, c, done] () {
		for {
			_ = <-c or {
				done <- true
				return
			}
			stop.v = true
		}
	}()
	for !stop.v {
		c <- true
	}
	c.close()
	_ = <-done
}

fn test_race_for_incr() {
	done := chan bool{}
	c := chan bool{}
	mut x := &Cell[int]{}
	spawn fn [mut x, c, done] () {
		for {
			_ = <-c or {
				done <- true
				return
			}
			x.v++
		}
	}()
	for i := 0; i < 10; x.v++ {
		i++
		c <- true
	}
	c.close()
	_ = <-done
}

fn test_no_race_for_incr() {
	done := chan bool{}
	mut x := &Cell[int]{}
	spawn fn [mut x, done] () {
		x.v++
		done <- true
	}()
	for i := 0; i < 0; x.v++ {
	}
	_ = <-done
}

fn test_race_plus() {
	x := &Cell[int]{}
	mut y := &Cell[int]{}
	z := &Cell[int]{}
	_ = y.v
	ch := chan int{cap: 2}

	spawn fn [x, mut y, z, ch] () {
		y.v = x.v + z.v
		ch <- 1
	}()
	spawn fn [x, mut y, z, ch] () {
		y.v = x.v + z.v + z.v
		ch <- 1
	}()
	_ = <-ch
	_ = <-ch
}

fn test_race_plus2() {
	mut x := &Cell[int]{}
	mut y := &Cell[int]{}
	z := &Cell[int]{}
	_ = y.v
	ch := chan int{cap: 2}

	spawn fn [mut x, ch] () {
		x.v = 1
		ch <- 1
	}()
	spawn fn [x, mut y, z, ch] () {
		y.v = +x.v + z.v
		ch <- 1
	}()
	_ = <-ch
	_ = <-ch
}

fn test_no_race_plus() {
	x := &Cell[int]{}
	mut y := &Cell[int]{}
	z := &Cell[int]{}
	mut f := &Cell[int]{}
	_ = x.v + y.v + f.v
	ch := chan int{cap: 2}

	spawn fn [x, mut y, z, ch] () {
		y.v = x.v + z.v
		ch <- 1
	}()
	spawn fn [x, z, mut f, ch] () {
		f.v = z.v + x.v
		ch <- 1
	}()
	_ = <-ch
	_ = <-ch
}

fn test_race_complement() {
	mut x := &Cell[int]{}
	mut y := &Cell[int]{}
	z := &Cell[int]{}
	_ = x.v
	ch := chan int{cap: 2}

	spawn fn [mut x, y, ch] () {
		x.v = ~y.v
		ch <- 1
	}()
	spawn fn [mut y, z, ch] () {
		y.v = ~z.v
		ch <- 1
	}()
	_ = <-ch
	_ = <-ch
}

fn test_race_div() {
	mut x := &Cell[int]{}
	mut y := &Cell[int]{}
	z := &Cell[int]{}
	_ = x.v
	ch := chan int{cap: 2}

	spawn fn [mut x, y, z, ch] () {
		x.v = y.v / (z.v + 1)
		ch <- 1
	}()
	spawn fn [mut y, z, ch] () {
		y.v = z.v
		ch <- 1
	}()
	_ = <-ch
	_ = <-ch
}

fn test_race_div_const() {
	mut x := &Cell[u32]{}
	mut y := &Cell[u32]{}
	z := &Cell[u32]{}
	_ = x.v
	ch := chan int{cap: 2}

	spawn fn [mut x, y, ch] () {
		x.v = y.v / 3 // involves only a HMUL node
		ch <- 1
	}()
	spawn fn [mut y, z, ch] () {
		y.v = z.v
		ch <- 1
	}()
	_ = <-ch
	_ = <-ch
}

fn test_race_mod() {
	mut x := &Cell[int]{}
	mut y := &Cell[int]{}
	z := &Cell[int]{}
	_ = x.v
	ch := chan int{cap: 2}

	spawn fn [mut x, y, z, ch] () {
		x.v = y.v % (z.v + 1)
		ch <- 1
	}()
	spawn fn [mut y, z, ch] () {
		y.v = z.v
		ch <- 1
	}()
	_ = <-ch
	_ = <-ch
}

fn test_race_mod_const() {
	mut x := &Cell[int]{}
	mut y := &Cell[int]{}
	z := &Cell[int]{}
	_ = x.v
	ch := chan int{cap: 2}

	spawn fn [mut x, y, ch] () {
		x.v = y.v % 3
		ch <- 1
	}()
	spawn fn [mut y, z, ch] () {
		y.v = z.v
		ch <- 1
	}()
	_ = <-ch
	_ = <-ch
}

fn test_race_rotate() {
	mut x := &Cell[u32]{}
	mut y := &Cell[u32]{}
	z := &Cell[u32]{}
	_ = x.v
	ch := chan int{cap: 2}

	spawn fn [mut x, y, ch] () {
		x.v = y.v << 12 | y.v >> 20
		ch <- 1
	}()
	spawn fn [mut y, z, ch] () {
		y.v = z.v
		ch <- 1
	}()
	_ = <-ch
	_ = <-ch
}

// the constants of test_no_race_enough_registers, from erf.go
const sa1 = 1.0
const sa2 = 2.0
const sa3 = 3.0
const sa4 = 4.0
const sa5 = 5.0
const sa6 = 6.0
const sa7 = 7.0
const sa8 = 8.0

// May crash if the instrumentation is reckless.
fn test_no_race_enough_registers() {
	// from erf.go
	mut s := 0.0
	mut big_s := 0.0
	s = 3.1415
	big_s = 1 + s * (sa1 + s * (sa2 + s * (sa3 + s * (sa4 + s * (sa5 + s * (sa6 + s * (sa7 +
		s * sa8)))))))
	s = big_s
}

// empty_func should not be inlined.
@[noinline]
fn empty_func(x int) {
	if false {
		println(x)
	}
}

fn test_race_func_argument() {
	mut x := &Cell[int]{}
	ch := chan bool{cap: 1}
	spawn fn [x, ch] () {
		empty_func(x.v)
		ch <- true
	}()
	x.v = 1
	_ = <-ch
}

fn test_race_func_argument2() {
	mut x := &Cell[int]{}
	ch := chan bool{cap: 2}
	spawn fn [mut x, ch] () {
		x.v = 42
		ch <- true
	}()
	spawn fn [ch] (y int) {
		ch <- true
	}(x.v)
	_ = <-ch
	_ = <-ch
}

fn test_race_sprint() {
	mut x := &Cell[int]{}
	ch := chan bool{cap: 1}
	spawn fn [x, ch] () {
		_ = '${x.v}'
		ch <- true
	}()
	x.v = 1
	_ = <-ch
}

fn test_race_array_copy() {
	ch := chan bool{cap: 1}
	mut a := &Cell[[5]int]{}
	spawn fn [mut a, ch] () {
		a.v[3] = 1
		ch <- true
	}()
	a.v = [1, 2, 3, 4, 5]!
	_ = <-ch
}

// the types of test_race_nested_array_copy
type Point32 = [2][2][2][2][2]Point
type Point1024 = [2][2][2][2][2]Point32
type Point32k = [2][2][2][2][2]Point1024
type Point1M = [2][2][2][2][2]Point32k
type Point1MCell = Cell[Point1M]

// new_point1m_cell allocates a zeroed Point1M directly on the heap.
fn new_point1m_cell() &Point1MCell {
	return unsafe { &Point1MCell(voidptr(vcalloc(int(sizeof(Point1MCell))))) }
}

// Blows up a naive compiler.
fn test_race_nested_array_copy() {
	ch := chan bool{cap: 1}
	// Like in Go, a and b are on the heap. `&Cell[Point1M]{}` would build each 16 MB value on
	// the stack first, which is larger than a Linux main thread stack.
	mut a := new_point1m_cell()
	b := new_point1m_cell()
	spawn fn [mut a, ch] () {
		a.v[0][1][0][1][0][1][0][1][0][1][0][1][0][1][0][1][0][1][0][1].y = 1
		ch <- true
	}()
	a.v = b.v
	_ = <-ch
}

fn test_race_struct_rw() {
	mut p := &Cell[Point]{
		v: Point{
			x: 0
			y: 0
		}
	}
	ch := chan bool{cap: 1}
	spawn fn [mut p, ch] () {
		p.v = Point{
			x: 1
			y: 1
		}
		ch <- true
	}()
	q := p.v
	_ = <-ch
	p.v = q
}

fn test_race_struct_field_rw1() {
	mut p := &Cell[Point]{
		v: Point{
			x: 0
			y: 0
		}
	}
	ch := chan bool{cap: 1}
	spawn fn [mut p, ch] () {
		p.v.x = 1
		ch <- true
	}()
	_ = p.v.x
	_ = <-ch
}

fn test_no_race_struct_field_rw1() {
	// Same struct, different variables, no
	// pointers. The layout is known (at compile time?) ->
	// no read on p
	// writes on x and y
	mut p := &Cell[Point]{
		v: Point{
			x: 0
			y: 0
		}
	}
	ch := chan bool{cap: 1}
	spawn fn [mut p, ch] () {
		p.v.x = 1
		ch <- true
	}()
	p.v.y = 1
	_ = <-ch
	_ = p.v
}

fn test_no_race_struct_field_rw2() {
	// Same as NoRaceStructFieldRW1
	// but p is a pointer, so there is a read on p
	mut p := &Cell[Point]{
		v: Point{
			x: 0
			y: 0
		}
	}
	ch := chan bool{cap: 1}
	spawn fn [mut p, ch] () {
		p.v.x = 1
		ch <- true
	}()
	p.v.y = 1
	_ = <-ch
	_ = p.v
}

fn test_race_struct_field_rw2() {
	mut p := &Point{
		x: 0
		y: 0
	}
	ch := chan bool{cap: 1}
	spawn fn [mut p, ch] () {
		p.x = 1
		ch <- true
	}()
	_ = p.x
	_ = <-ch
}

fn test_race_struct_field_rw3() {
	mut p := &Cell[NamedPoint]{
		v: NamedPoint{
			name: 'a'
			p:    Point{
				x: 0
				y: 0
			}
		}
	}
	ch := chan bool{cap: 1}
	spawn fn [mut p, ch] () {
		p.v.p.x = 1
		ch <- true
	}()
	_ = p.v.p.x
	_ = <-ch
}

fn test_race_eface_ww() {
	// Go's `var a, b any` starts as nil, the zero value of an interface.
	mut a := &Cell[Any]{}
	b := &Cell[Any]{}
	ch := chan bool{cap: 1}
	spawn fn [mut a, ch] () {
		a.v = 1
		ch <- true
	}()
	a.v = 2
	_ = <-ch
	_, _ = a.v, b.v
}
