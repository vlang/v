@[has_globals]
module main

// Translated from Go's src/runtime/race/testdata/slice_test.go, see ../README.md.
import sync
import time

struct Cell[T] {
mut:
	v T
}

fn main() {
	run('test_race_slice_rw', test_race_slice_rw)
	run('test_no_race_slice_rw', test_no_race_slice_rw)
	run('test_race_slice_ww', test_race_slice_ww)
	run('test_no_race_array_ww', test_no_race_array_ww)
	run('test_race_array_ww', test_race_array_ww)
	run('test_no_race_slice_write_len', test_no_race_slice_write_len)
	run('test_no_race_slice_write_cap', test_no_race_slice_write_cap)
	run('test_race_slice_copy_read', test_race_slice_copy_read)
	run('test_no_race_slice_write_copy', test_no_race_slice_write_copy)
	run('test_race_slice_copy_write2', test_race_slice_copy_write2)
	run('test_race_slice_copy_write3', test_race_slice_copy_write3)
	run('test_no_race_slice_copy_read', test_no_race_slice_copy_read)
	run('test_race_pointer_slice_copy_read', test_race_pointer_slice_copy_read)
	run('test_no_race_pointer_slice_write_copy', test_no_race_pointer_slice_write_copy)
	run('test_race_pointer_slice_copy_write2', test_race_pointer_slice_copy_write2)
	run('test_no_race_pointer_slice_copy_read', test_no_race_pointer_slice_copy_read)
	run('test_no_race_slice_write_slice2', test_no_race_slice_write_slice2)
	run('test_race_slice_write_slice', test_race_slice_write_slice)
	run('test_no_race_slice_write_slice', test_no_race_slice_write_slice)
	run('test_no_race_slice_len_cap', test_no_race_slice_len_cap)
	run('test_no_race_struct_slices_range_write', test_no_race_struct_slices_range_write)
	run('test_race_slice_different', test_race_slice_different)
	run('test_race_slice_range_write', test_race_slice_range_write)
	run('test_no_race_slice_range_write', test_no_race_slice_range_write)
	run('test_race_slice_range_append', test_race_slice_range_append)
	run('test_no_race_slice_range_append', test_no_race_slice_range_append)
	run('test_race_slice_var_write', test_race_slice_var_write)
	run('test_race_slice_var_read', test_race_slice_var_read)
	run('test_race_slice_var_range', test_race_slice_var_range)
	run('test_race_slice_var_append', test_race_slice_var_append)
	run('test_race_slice_var_copy', test_race_slice_var_copy)
	run('test_race_slice_var_copy2', test_race_slice_var_copy2)
	run('test_race_slice_append', test_race_slice_append)
	run('test_race_slice_append_write', test_race_slice_append_write)
	run('test_race_slice_append_slice', test_race_slice_append_slice)
	run('test_race_slice_append_slice2', test_race_slice_append_slice2)
	run('test_race_slice_append_string', test_race_slice_append_string)
	run('test_race_pointer_slice_append', test_race_pointer_slice_append)
	run('test_race_pointer_slice_append_write', test_race_pointer_slice_append_write)
	run('test_race_pointer_slice_append_slice', test_race_pointer_slice_append_slice)
	run('test_race_pointer_slice_append_slice2', test_race_pointer_slice_append_slice2)
	run('test_no_race_slice_index_access', test_no_race_slice_index_access)
	run('test_no_race_slice_index_access2', test_no_race_slice_index_access2)
	run('test_race_slice_index_access', test_race_slice_index_access)
	run('test_race_slice_index_access2', test_race_slice_index_access2)
	run('test_race_slice_byte_to_string', test_race_slice_byte_to_string)
	run('test_race_slice_rune_to_string', test_race_slice_rune_to_string)
	run('test_race_concat_string', test_race_concat_string)
	run('test_race_compare_string', test_race_compare_string)
	run('test_race_slice3', test_race_slice3)
	run('test_race_slice4', test_race_slice4)
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

// new_int allocates a zero int on the heap, like Go's `new(int)`.
fn new_int() &int {
	mut x := 0
	return &x
}

// Go closures capture slice variables by reference, so a goroutine and the test share the
// slice header (data/len/cap) as well as the elements: the slices captured by the closures
// below are kept in a heap `Cell`. A Go `_ = append(s, x)` does not change `s`: it is
// translated as an append to a copy of the header (`mut t := unsafe { s.v }` `t << x`),
// which writes into the shared buffer when it has room, and copies the elements to a new
// buffer otherwise, exactly like Go's append.

fn test_race_slice_rw() {
	ch := chan bool{cap: 1}
	mut a := &Cell[[]int]{
		v: []int{len: 2}
	}
	spawn fn [mut a, ch] () {
		a.v[1] = 1
		ch <- true
	}()
	_ = a.v[1]
	_ = <-ch
}

fn test_no_race_slice_rw() {
	ch := chan bool{cap: 1}
	mut a := &Cell[[]int]{
		v: []int{len: 2}
	}
	spawn fn [mut a, ch] () {
		a.v[0] = 1
		ch <- true
	}()
	_ = a.v[1]
	_ = <-ch
}

fn test_race_slice_ww() {
	mut a := &Cell[[]int]{
		v: []int{len: 10}
	}
	ch := chan bool{cap: 1}
	spawn fn [mut a, ch] () {
		a.v[1] = 1
		ch <- true
	}()
	a.v[1] = 2
	_ = <-ch
}

fn test_no_race_array_ww() {
	mut a := &Cell[[5]int]{}
	ch := chan bool{cap: 1}
	spawn fn [mut a, ch] () {
		a.v[0] = 1
		ch <- true
	}()
	a.v[1] = 2
	_ = <-ch
}

fn test_race_array_ww() {
	mut a := &Cell[[5]int]{}
	ch := chan bool{cap: 1}
	spawn fn [mut a, ch] () {
		a.v[1] = 1
		ch <- true
	}()
	a.v[1] = 2
	_ = <-ch
}

fn test_no_race_slice_write_len() {
	ch := chan bool{cap: 1}
	mut a := &Cell[[]bool]{
		v: []bool{len: 1}
	}
	spawn fn [mut a, ch] () {
		a.v[0] = true
		ch <- true
	}()
	_ = a.v.len
	_ = <-ch
}

fn test_no_race_slice_write_cap() {
	ch := chan bool{cap: 1}
	mut a := &Cell[[]u64]{
		v: []u64{len: 100}
	}
	spawn fn [mut a, ch] () {
		a.v[50] = 123
		ch <- true
	}()
	_ = a.v.cap
	_ = <-ch
}

fn test_race_slice_copy_read() {
	ch := chan bool{cap: 1}
	mut a := &Cell[[]int]{
		v: []int{len: 10}
	}
	b := []int{len: 10}
	spawn fn [a, ch] () {
		_ = a.v[5]
		ch <- true
	}()
	copy(mut a.v, b)
	_ = <-ch
}

fn test_no_race_slice_write_copy() {
	ch := chan bool{cap: 1}
	mut a := &Cell[[]int]{
		v: []int{len: 10}
	}
	b := []int{len: 10}
	spawn fn [mut a, ch] () {
		a.v[5] = 1
		ch <- true
	}()
	copy(mut a.v[..5], b[..5])
	_ = <-ch
}

fn test_race_slice_copy_write2() {
	ch := chan bool{cap: 1}
	mut a := []int{len: 10}
	mut b := &Cell[[]int]{
		v: []int{len: 10}
	}
	spawn fn [mut b, ch] () {
		b.v[5] = 1
		ch <- true
	}()
	copy(mut a, b.v)
	_ = <-ch
}

fn test_race_slice_copy_write3() {
	ch := chan bool{cap: 1}
	mut a := &Cell[[]u8]{
		v: []u8{len: 10}
	}
	spawn fn [mut a, ch] () {
		a.v[7] = 1
		ch <- true
	}()
	copy(mut a.v, 'qwertyqwerty')
	_ = <-ch
}

fn test_no_race_slice_copy_read() {
	ch := chan bool{cap: 1}
	mut a := []int{len: 10}
	b := &Cell[[]int]{
		v: []int{len: 10}
	}
	spawn fn [b, ch] () {
		_ = b.v[5]
		ch <- true
	}()
	copy(mut a, b.v)
	_ = <-ch
}

fn test_race_pointer_slice_copy_read() {
	ch := chan bool{cap: 1}
	mut a := &Cell[[]&int]{
		v: unsafe { []&int{len: 10} }
	}
	b := unsafe { []&int{len: 10} }
	spawn fn [a, ch] () {
		_ = a.v[5]
		ch <- true
	}()
	copy(mut a.v, b)
	_ = <-ch
}

fn test_no_race_pointer_slice_write_copy() {
	ch := chan bool{cap: 1}
	mut a := &Cell[[]&int]{
		v: unsafe { []&int{len: 10} }
	}
	b := unsafe { []&int{len: 10} }
	spawn fn [mut a, ch] () {
		a.v[5] = new_int()
		ch <- true
	}()
	copy(mut a.v[..5], b[..5])
	_ = <-ch
}

fn test_race_pointer_slice_copy_write2() {
	ch := chan bool{cap: 1}
	mut a := unsafe { []&int{len: 10} }
	mut b := &Cell[[]&int]{
		v: unsafe { []&int{len: 10} }
	}
	spawn fn [mut b, ch] () {
		b.v[5] = new_int()
		ch <- true
	}()
	copy(mut a, b.v)
	_ = <-ch
}

fn test_no_race_pointer_slice_copy_read() {
	ch := chan bool{cap: 1}
	mut a := unsafe { []&int{len: 10} }
	b := &Cell[[]&int]{
		v: unsafe { []&int{len: 10} }
	}
	spawn fn [b, ch] () {
		_ = b.v[5]
		ch <- true
	}()
	copy(mut a, b.v)
	_ = <-ch
}

fn test_no_race_slice_write_slice2() {
	ch := chan bool{cap: 1}
	mut a := &Cell[[]f64]{
		v: []f64{len: 10}
	}
	spawn fn [mut a, ch] () {
		a.v[2] = 1.0
		ch <- true
	}()
	_ = a.v[0..5]
	_ = <-ch
}

fn test_race_slice_write_slice() {
	ch := chan bool{cap: 1}
	mut a := &Cell[[]f64]{
		v: []f64{len: 10}
	}
	spawn fn [mut a, ch] () {
		a.v[2] = 1.0
		ch <- true
	}()
	a.v = a.v[5..10]
	_ = <-ch
}

fn test_no_race_slice_write_slice() {
	ch := chan bool{cap: 1}
	mut a := &Cell[[]f64]{
		v: []f64{len: 10}
	}
	spawn fn [mut a, ch] () {
		a.v[2] = 1.0
		ch <- true
	}()
	_ = a.v[5..10]
	_ = <-ch
}

// Empty is Go's `struct{}`.
struct Empty {}

fn test_no_race_slice_len_cap() {
	ch := chan bool{cap: 1}
	a := &Cell[[]Empty]{
		v: []Empty{len: 10}
	}
	spawn fn [a, ch] () {
		_ = a.v.len
		ch <- true
	}()
	_ = a.v.cap
	_ = <-ch
}

// Str is declared inside TestNoRaceStructSlicesRangeWrite in Go.
struct Str {
mut:
	a []int
	b []int
}

fn test_no_race_struct_slices_range_write() {
	ch := chan bool{cap: 1}
	mut s := &Str{}
	s.a = []int{len: 10}
	s.b = []int{len: 10}
	spawn fn [s, ch] () {
		for _ in s.a {
		}
		ch <- true
	}()
	s.b[5] = 5
	_ = <-ch
}

fn test_race_slice_different() {
	c := chan bool{cap: 1}
	mut s := &Cell[[]int]{
		v: []int{len: 10}
	}
	mut s2 := unsafe { s.v }
	spawn fn [mut s, c] () {
		s.v[3] = 3
		c <- true
	}()
	// false negative because s2 is PAUTO w/o PHEAP
	// so we do not instrument it
	s2[3] = 3
	_ = <-c
}

fn test_race_slice_range_write() {
	c := chan bool{cap: 1}
	mut s := &Cell[[]int]{
		v: []int{len: 10}
	}
	spawn fn [mut s, c] () {
		s.v[3] = 3
		c <- true
	}()
	for v in s.v {
		_ = v
	}
	_ = <-c
}

fn test_no_race_slice_range_write() {
	c := chan bool{cap: 1}
	mut s := &Cell[[]int]{
		v: []int{len: 10}
	}
	spawn fn [mut s, c] () {
		s.v[3] = 3
		c <- true
	}()
	for _ in s.v {
	}
	_ = <-c
}

fn test_race_slice_range_append() {
	c := chan bool{cap: 1}
	mut s := &Cell[[]int]{
		v: []int{len: 10}
	}
	spawn fn [mut s, c] () {
		s.v << 3
		c <- true
	}()
	for _ in s.v {
	}
	_ = <-c
}

fn test_no_race_slice_range_append() {
	c := chan bool{cap: 1}
	s := &Cell[[]int]{
		v: []int{len: 10}
	}
	spawn fn [s, c] () {
		mut t := unsafe { s.v }
		t << 3
		c <- true
	}()
	for _ in s.v {
	}
	_ = <-c
}

fn test_race_slice_var_write() {
	c := chan bool{cap: 1}
	mut s := &Cell[[]int]{
		v: []int{len: 10}
	}
	spawn fn [mut s, c] () {
		s.v[3] = 3
		c <- true
	}()
	s.v = []int{len: 20}
	_ = <-c
}

fn test_race_slice_var_read() {
	c := chan bool{cap: 1}
	mut s := &Cell[[]int]{
		v: []int{len: 10}
	}
	spawn fn [s, c] () {
		_ = s.v[3]
		c <- true
	}()
	s.v = []int{len: 20}
	_ = <-c
}

fn test_race_slice_var_range() {
	c := chan bool{cap: 1}
	mut s := &Cell[[]int]{
		v: []int{len: 10}
	}
	spawn fn [s, c] () {
		for _ in s.v {
		}
		c <- true
	}()
	s.v = []int{len: 20}
	_ = <-c
}

fn test_race_slice_var_append() {
	c := chan bool{cap: 1}
	mut s := &Cell[[]int]{
		v: []int{len: 10}
	}
	spawn fn [s, c] () {
		mut t := unsafe { s.v }
		t << 10
		c <- true
	}()
	s.v = []int{len: 20}
	_ = <-c
}

fn test_race_slice_var_copy() {
	c := chan bool{cap: 1}
	mut s := &Cell[[]int]{
		v: []int{len: 10}
	}
	spawn fn [mut s, c] () {
		s2 := []int{len: 10}
		copy(mut s.v, s2)
		c <- true
	}()
	s.v = []int{len: 20}
	_ = <-c
}

fn test_race_slice_var_copy2() {
	c := chan bool{cap: 1}
	mut s := &Cell[[]int]{
		v: []int{len: 10}
	}
	spawn fn [s, c] () {
		mut s2 := []int{len: 10}
		copy(mut s2, s.v)
		c <- true
	}()
	s.v = []int{len: 20}
	_ = <-c
}

fn test_race_slice_append() {
	c := chan bool{cap: 1}
	s := &Cell[[]int]{
		v: []int{len: 10, cap: 20}
	}
	spawn fn [s, c] () {
		mut t := unsafe { s.v }
		t << 1
		c <- true
	}()
	mut t := unsafe { s.v }
	t << 2
	_ = <-c
}

fn test_race_slice_append_write() {
	c := chan bool{cap: 1}
	mut s := &Cell[[]int]{
		v: []int{len: 10}
	}
	spawn fn [s, c] () {
		mut t := unsafe { s.v }
		t << 1
		c <- true
	}()
	s.v[0] = 42
	_ = <-c
}

fn test_race_slice_append_slice() {
	c := chan bool{cap: 1}
	mut s := &Cell[[]int]{
		v: []int{len: 10}
	}
	spawn fn [s, c] () {
		s2 := []int{len: 10}
		mut t := unsafe { s.v }
		t << s2
		c <- true
	}()
	s.v[0] = 42
	_ = <-c
}

fn test_race_slice_append_slice2() {
	c := chan bool{cap: 1}
	s := &Cell[[]int]{
		v: []int{len: 10}
	}
	mut s2foobar := &Cell[[]int]{
		v: []int{len: 10}
	}
	spawn fn [s, s2foobar, c] () {
		mut t := unsafe { s.v }
		t << s2foobar.v
		c <- true
	}()
	s2foobar.v[5] = 42
	_ = <-c
}

fn test_race_slice_append_string() {
	c := chan bool{cap: 1}
	mut s := &Cell[[]u8]{
		v: []u8{len: 10}
	}
	spawn fn [s, c] () {
		mut t := unsafe { s.v }
		t << 'qwerty'.bytes()
		c <- true
	}()
	s.v[0] = 42
	_ = <-c
}

fn test_race_pointer_slice_append() {
	c := chan bool{cap: 1}
	s := &Cell[[]&int]{
		v: unsafe { []&int{len: 10, cap: 20} }
	}
	spawn fn [s, c] () {
		mut t := unsafe { s.v }
		t << new_int()
		c <- true
	}()
	mut t := unsafe { s.v }
	t << new_int()
	_ = <-c
}

fn test_race_pointer_slice_append_write() {
	c := chan bool{cap: 1}
	mut s := &Cell[[]&int]{
		v: unsafe { []&int{len: 10} }
	}
	spawn fn [s, c] () {
		mut t := unsafe { s.v }
		t << new_int()
		c <- true
	}()
	s.v[0] = new_int()
	_ = <-c
}

fn test_race_pointer_slice_append_slice() {
	c := chan bool{cap: 1}
	mut s := &Cell[[]&int]{
		v: unsafe { []&int{len: 10} }
	}
	spawn fn [s, c] () {
		s2 := unsafe { []&int{len: 10} }
		mut t := unsafe { s.v }
		t << s2
		c <- true
	}()
	s.v[0] = new_int()
	_ = <-c
}

fn test_race_pointer_slice_append_slice2() {
	c := chan bool{cap: 1}
	s := &Cell[[]&int]{
		v: unsafe { []&int{len: 10} }
	}
	mut s2foobar := &Cell[[]&int]{
		v: unsafe { []&int{len: 10} }
	}
	spawn fn [s, s2foobar, c] () {
		mut t := unsafe { s.v }
		t << s2foobar.v
		c <- true
	}()
	eprintln('WRITE: ${voidptr(unsafe { &s2foobar.v[5] })}')
	s2foobar.v[5] = unsafe { nil }
	_ = <-c
}

fn test_no_race_slice_index_access() {
	c := chan bool{cap: 1}
	mut s := []int{len: 10}
	v := &Cell[int]{}
	spawn fn [v, c] () {
		_ = v.v
		c <- true
	}()
	s[v.v] = 1
	_ = <-c
}

fn test_no_race_slice_index_access2() {
	c := chan bool{cap: 1}
	s := []int{len: 10}
	v := &Cell[int]{}
	spawn fn [v, c] () {
		_ = v.v
		c <- true
	}()
	_ = s[v.v]
	_ = <-c
}

fn test_race_slice_index_access() {
	c := chan bool{cap: 1}
	mut s := []int{len: 10}
	mut v := &Cell[int]{}
	spawn fn [mut v, c] () {
		v.v = 1
		c <- true
	}()
	s[v.v] = 1
	_ = <-c
}

fn test_race_slice_index_access2() {
	c := chan bool{cap: 1}
	s := []int{len: 10}
	mut v := &Cell[int]{}
	spawn fn [mut v, c] () {
		v.v = 1
		c <- true
	}()
	_ = s[v.v]
	_ = <-c
}

fn test_race_slice_byte_to_string() {
	c := chan string{}
	mut s := &Cell[[]u8]{
		v: []u8{len: 10}
	}
	spawn fn [s, c] () {
		c <- s.v.bytestr()
	}()
	s.v[0] = 42
	_ = <-c
}

fn test_race_slice_rune_to_string() {
	c := chan string{}
	mut s := &Cell[[]rune]{
		v: []rune{len: 10}
	}
	spawn fn [s, c] () {
		c <- s.v.string()
	}()
	s.v[9] = 42
	_ = <-c
}

fn test_race_concat_string() {
	mut s := &Cell[string]{
		v: 'hello'
	}
	c := chan string{cap: 1}
	spawn fn [s, c] () {
		c <- s.v + ' world'
	}()
	s.v = 'world'
	_ = <-c
}

fn test_race_compare_string() {
	mut s1 := &Cell[string]{
		v: 'hello'
	}
	s2 := &Cell[string]{
		v: 'world'
	}
	c := chan bool{cap: 1}
	spawn fn [s1, s2, c] () {
		c <- s1.v == s2.v
	}()
	s1.v = s2.v
	_ = <-c
}

fn test_race_slice3() {
	done := chan bool{}
	x := []int{len: 10}
	mut i := &Cell[int]{
		v: 2
	}
	spawn fn [mut i, done] () {
		i.v = 3
		done <- true
	}()
	// V has no full slice expression like Go's `x[:1:i]`; slicing `x[..i]` first reads `i`
	// as a slice bound and checks `1 <= i <= cap(x)` like Go does.
	_ = x[..i.v][..1]
	_ = <-done
}

__global saved string

fn test_race_slice4() {
	// See issue 36794.
	mut data := &Cell[[]u8]{
		v: 'hello there'.bytes()
	}
	mut wg := sync.new_waitgroup()
	wg.add(1)
	spawn fn [data, mut wg] () {
		_ = data.v.bytestr()
		wg.done()
	}()
	copy(mut data.v, data.v[2..])
	wg.wait()
}
