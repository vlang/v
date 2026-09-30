// Translated from Go's src/runtime/race/testdata/comp_test.go, see ../README.md.
import time

struct Cell[T] {
mut:
	v T
}

fn main() {
	run('test_no_race_comp', test_no_race_comp)
	run('test_no_race_comp2', test_no_race_comp2)
	run('test_race_comp', test_race_comp)
	run('test_race_comp2', test_race_comp2)
	run('test_race_comp3', test_race_comp3)
	run('test_race_comp_array', test_race_comp_array)
	run('test_race_conv1', test_race_conv1)
	run('test_race_conv2', test_race_conv2)
	run('test_race_conv3', test_race_conv3)
	run('test_race_conv4', test_race_conv4)
	run('test_no_race_comp_ptr', test_no_race_comp_ptr)
	run('test_no_race_comp_ptr2', test_no_race_comp_ptr2)
	run('test_race_comp_ptr', test_race_comp_ptr)
	run('test_race_comp_ptr2', test_race_comp_ptr2)
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

struct P {
mut:
	x int
	y int
}

struct S {
mut:
	s1 P
	s2 P
}

fn test_no_race_comp() {
	c := chan bool{cap: 1}
	mut s := &Cell[S]{}
	spawn fn [mut s, c] () {
		s.v.s2.x = 1
		c <- true
	}()
	s.v.s2.y = 2
	_ = <-c
}

fn test_no_race_comp2() {
	c := chan bool{cap: 1}
	mut s := &Cell[S]{}
	spawn fn [mut s, c] () {
		s.v.s1.x = 1
		c <- true
	}()
	s.v.s1.y = 2
	_ = <-c
}

fn test_race_comp() {
	c := chan bool{cap: 1}
	mut s := &Cell[S]{}
	spawn fn [mut s, c] () {
		s.v.s2.y = 1
		c <- true
	}()
	s.v.s2.y = 2
	_ = <-c
}

fn test_race_comp2() {
	c := chan bool{cap: 1}
	mut s := &Cell[S]{}
	spawn fn [mut s, c] () {
		s.v.s1.x = 1
		c <- true
	}()
	s.v = S{}
	_ = <-c
}

fn test_race_comp3() {
	c := chan bool{cap: 1}
	mut s := &Cell[S]{}
	spawn fn [mut s, c] () {
		s.v.s2.y = 1
		c <- true
	}()
	s.v = S{}
	_ = <-c
}

fn test_race_comp_array() {
	c := chan bool{cap: 1}
	mut s := []S{len: 10}
	mut x := &Cell[int]{
		v: 4
	}
	spawn fn [mut s, x, c] () {
		s[x.v].s2.y = 1
		c <- true
	}()
	x.v = 5
	_ = <-c
}

type P2 = P
type S2 = S

fn test_race_conv1() {
	c := chan bool{cap: 1}
	mut p := &Cell[P2]{}
	spawn fn [mut p, c] () {
		p.v.x = 1
		c <- true
	}()
	_ = P(p.v).x
	_ = <-c
}

fn test_race_conv2() {
	c := chan bool{cap: 1}
	mut p := &Cell[P2]{}
	spawn fn [mut p, c] () {
		p.v.x = 1
		c <- true
	}()
	ptr := &p.v
	_ = P(*ptr).x
	_ = <-c
}

fn test_race_conv3() {
	c := chan bool{cap: 1}
	mut s := &Cell[S2]{}
	spawn fn [mut s, c] () {
		s.v.s1.x = 1
		c <- true
	}()
	_ = P2(S(s.v).s1).x
	_ = <-c
}

// Go's field `V` is `v` here, as V field names are lower case.
struct X {
mut:
	v [4]P
}

type X2 = X

fn test_race_conv4() {
	c := chan bool{cap: 1}
	mut x := &Cell[X2]{}
	spawn fn [mut x, c] () {
		x.v.v[1].x = 1
		c <- true
	}()
	_ = P2(X(x.v).v[1]).x
	_ = <-c
}

struct Ptr {
mut:
	s1 &P
	s2 &P
}

fn test_no_race_comp_ptr() {
	c := chan bool{cap: 1}
	mut p := &Cell[Ptr]{
		v: Ptr{
			s1: &P{}
			s2: &P{}
		}
	}
	spawn fn [mut p, c] () {
		p.v.s1.x = 1
		c <- true
	}()
	p.v.s1.y = 2
	_ = <-c
}

fn test_no_race_comp_ptr2() {
	c := chan bool{cap: 1}
	mut p := &Cell[Ptr]{
		v: Ptr{
			s1: &P{}
			s2: &P{}
		}
	}
	spawn fn [mut p, c] () {
		p.v.s1.x = 1
		c <- true
	}()
	_ = p.v
	_ = <-c
}

fn test_race_comp_ptr() {
	c := chan bool{cap: 1}
	mut p := &Cell[Ptr]{
		v: Ptr{
			s1: &P{}
			s2: &P{}
		}
	}
	spawn fn [mut p, c] () {
		p.v.s2.x = 1
		c <- true
	}()
	p.v.s2.x = 2
	_ = <-c
}

fn test_race_comp_ptr2() {
	c := chan bool{cap: 1}
	mut p := &Cell[Ptr]{
		v: Ptr{
			s1: &P{}
			s2: &P{}
		}
	}
	spawn fn [mut p, c] () {
		p.v.s2.x = 1
		c <- true
	}()
	p.v.s2 = &P{}
	_ = <-c
}
