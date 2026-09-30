// Translated from Go's src/runtime/race/testdata/map_test.go, see ../README.md.
import time

struct Cell[T] {
mut:
	v T
}

fn main() {
	run('test_race_map_rw', test_race_map_rw)
	run('test_race_map_rw2', test_race_map_rw2)
	run('test_race_map_rw_array', test_race_map_rw_array)
	run('test_no_race_map_rr', test_no_race_map_rr)
	run('test_race_map_range', test_race_map_range)
	run('test_race_map_range2', test_race_map_range2)
	run('test_no_race_map_range_range', test_no_race_map_range_range)
	run('test_race_map_len', test_race_map_len)
	run('test_race_map_delete', test_race_map_delete)
	run('test_race_map_len_delete', test_race_map_len_delete)
	run('test_race_map_variable', test_race_map_variable)
	run('test_race_map_variable2', test_race_map_variable2)
	run('test_race_map_variable3', test_race_map_variable3)
	run('test_race_map_lookup_part_key', test_race_map_lookup_part_key)
	run('test_race_map_lookup_part_key2', test_race_map_lookup_part_key2)
	run('test_race_map_delete_part_key', test_race_map_delete_part_key)
	run('test_race_map_insert_part_key', test_race_map_insert_part_key)
	run('test_race_map_insert_part_val', test_race_map_insert_part_val)
	run('test_race_map_assign_multiple_return', test_race_map_assign_multiple_return)
	run('test_race_map_big_key_access1', test_race_map_big_key_access1)
	run('test_race_map_big_key_access2', test_race_map_big_key_access2)
	run('test_race_map_big_key_insert', test_race_map_big_key_insert)
	run('test_race_map_big_key_delete', test_race_map_big_key_delete)
	run('test_race_map_big_val_insert', test_race_map_big_val_insert)
	run('test_race_map_big_val_access1', test_race_map_big_val_access1)
	run('test_race_map_big_val_access2', test_race_map_big_val_access2)
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

fn test_race_map_rw() {
	mut m := &Cell[map[int]int]{}
	ch := chan bool{cap: 1}
	spawn fn [m, ch] () {
		_ = m.v[1]
		ch <- true
	}()
	m.v[1] = 1
	_ = <-ch
}

fn test_race_map_rw2() {
	mut m := &Cell[map[int]int]{}
	ch := chan bool{cap: 1}
	spawn fn [m, ch] () {
		_ = m.v[1] or { 0 }
		ch <- true
	}()
	m.v[1] = 1
	_ = <-ch
}

fn test_race_map_rw_array() {
	// Check instrumentation of unaddressable arrays (issue 4578).
	mut m := &Cell[map[int][2]int]{}
	ch := chan bool{cap: 1}
	spawn fn [m, ch] () {
		_ = m.v[1][1]
		ch <- true
	}()
	m.v[2] = [1, 2]!
	_ = <-ch
}

fn test_no_race_map_rr() {
	m := &Cell[map[int]int]{}
	ch := chan bool{cap: 1}
	spawn fn [m, ch] () {
		_ = m.v[1] or { 0 }
		ch <- true
	}()
	_ = m.v[1]
	_ = <-ch
}

fn test_race_map_range() {
	mut m := &Cell[map[int]int]{}
	ch := chan bool{cap: 1}
	spawn fn [m, ch] () {
		for _, _ in m.v {
		}
		ch <- true
	}()
	m.v[1] = 1
	_ = <-ch
}

fn test_race_map_range2() {
	mut m := &Cell[map[int]int]{}
	ch := chan bool{cap: 1}
	spawn fn [m, ch] () {
		for _, _ in m.v {
		}
		ch <- true
	}()
	m.v[1] = 1
	_ = <-ch
}

fn test_no_race_map_range_range() {
	mut m := &Cell[map[int]int]{}
	// now the map is not empty and range triggers an event
	// should work without this (as in other tests)
	// so it is suspicious if this test passes and others don't
	m.v[0] = 0
	ch := chan bool{cap: 1}
	spawn fn [m, ch] () {
		for _, _ in m.v {
		}
		ch <- true
	}()
	for _, _ in m.v {
	}
	_ = <-ch
}

fn test_race_map_len() {
	mut m := &Cell[map[string]bool]{}
	ch := chan bool{cap: 1}
	spawn fn [m, ch] () {
		_ = m.v.len
		ch <- true
	}()
	m.v[''] = true
	_ = <-ch
}

fn test_race_map_delete() {
	mut m := &Cell[map[string]bool]{}
	ch := chan bool{cap: 1}
	spawn fn [mut m, ch] () {
		m.v.delete('')
		ch <- true
	}()
	m.v[''] = true
	_ = <-ch
}

fn test_race_map_len_delete() {
	mut m := &Cell[map[string]bool]{}
	ch := chan bool{cap: 1}
	spawn fn [mut m, ch] () {
		m.v.delete('a')
		ch <- true
	}()
	_ = m.v.len
	_ = <-ch
}

fn test_race_map_variable() {
	ch := chan bool{cap: 1}
	mut m := &Cell[map[int]int]{}
	_ = m.v
	spawn fn [mut m, ch] () {
		m.v = map[int]int{}
		ch <- true
	}()
	m.v = map[int]int{}
	_ = <-ch
}

fn test_race_map_variable2() {
	ch := chan bool{cap: 1}
	mut m := &Cell[map[int]int]{}
	spawn fn [mut m, ch] () {
		m.v[1] = 1
		ch <- true
	}()
	m.v = map[int]int{}
	_ = <-ch
}

fn test_race_map_variable3() {
	ch := chan bool{cap: 1}
	mut m := &Cell[map[int]int]{}
	spawn fn [m, ch] () {
		_ = m.v[1]
		ch <- true
	}()
	m.v = map[int]int{}
	_ = <-ch
}

struct Big {
mut:
	x [17]i32
}

// V maps do not support struct keys (`map key type `Big` not supported`), so the
// map[Big]bool of the next four tests is keyed by Big's only field instead: `m[k.x]` reads
// all of `*k` for the key, like Go's `m[*k]` does.

fn test_race_map_lookup_part_key() {
	mut k := &Big{}
	m := map[[17]i32]bool{}
	ch := chan bool{cap: 1}
	spawn fn [mut k, ch] () {
		k.x[8] = 1
		ch <- true
	}()
	_ = m[k.x]
	_ = <-ch
}

fn test_race_map_lookup_part_key2() {
	mut k := &Big{}
	m := map[[17]i32]bool{}
	ch := chan bool{cap: 1}
	spawn fn [mut k, ch] () {
		k.x[8] = 1
		ch <- true
	}()
	_ = m[k.x] or { false }
	_ = <-ch
}

fn test_race_map_delete_part_key() {
	mut k := &Big{}
	mut m := map[[17]i32]bool{}
	ch := chan bool{cap: 1}
	spawn fn [mut k, ch] () {
		k.x[8] = 1
		ch <- true
	}()
	m.delete(k.x)
	_ = <-ch
}

fn test_race_map_insert_part_key() {
	mut k := &Big{}
	mut m := map[[17]i32]bool{}
	ch := chan bool{cap: 1}
	spawn fn [mut k, ch] () {
		k.x[8] = 1
		ch <- true
	}()
	m[k.x] = true
	_ = <-ch
}

fn test_race_map_insert_part_val() {
	mut v := &Big{}
	mut m := map[int]Big{}
	ch := chan bool{cap: 1}
	spawn fn [mut v, ch] () {
		v.x[8] = 1
		ch <- true
	}()
	m[1] = *v
	_ = <-ch
}

// Test for issue 7561.
fn test_race_map_assign_multiple_return() {
	// V's `none` stands for Go's nil error.
	connect := fn () (int, IError) {
		return 42, none
	}
	mut conns := &Cell[map[int][]int]{}
	conns.v[1] = [0]
	ch := chan bool{cap: 1}
	mut err := &Cell[IError]{
		v: none
	}
	_ = err.v
	spawn fn [connect, mut conns, mut err, ch] () {
		conns.v[1][0], err.v = connect()
		ch <- true
	}()
	x := conns.v[1][0]
	_ = x
	_ = <-ch
}

// BigKey and BigVal must be larger than 256 bytes,
// so that compiler stores them indirectly.
type BigKey = [1000]&int

struct BigVal {
mut:
	x int
	y [1000]&int
}

// new_int returns a pointer to a new zero int, like Go's new(int).
fn new_int() &int {
	x := 0
	return &x
}

fn test_race_map_big_key_access1() {
	m := map[BigKey]int{}
	mut k := &Cell[BigKey]{}
	ch := chan bool{cap: 1}
	spawn fn [m, k, ch] () {
		_ = m[k.v]
		ch <- true
	}()
	k.v[30] = new_int()
	_ = <-ch
}

fn test_race_map_big_key_access2() {
	m := map[BigKey]int{}
	mut k := &Cell[BigKey]{}
	ch := chan bool{cap: 1}
	spawn fn [m, k, ch] () {
		_ = m[k.v] or { 0 }
		ch <- true
	}()
	k.v[30] = new_int()
	_ = <-ch
}

fn test_race_map_big_key_insert() {
	mut m := map[BigKey]int{}
	mut k := &Cell[BigKey]{}
	ch := chan bool{cap: 1}
	spawn fn [mut m, k, ch] () {
		m[k.v] = 1
		ch <- true
	}()
	k.v[30] = new_int()
	_ = <-ch
}

fn test_race_map_big_key_delete() {
	mut m := map[BigKey]int{}
	mut k := &Cell[BigKey]{}
	ch := chan bool{cap: 1}
	spawn fn [mut m, k, ch] () {
		m.delete(k.v)
		ch <- true
	}()
	k.v[30] = new_int()
	_ = <-ch
}

fn test_race_map_big_val_insert() {
	mut m := map[int]BigVal{}
	mut v := &Cell[BigVal]{}
	ch := chan bool{cap: 1}
	spawn fn [mut m, v, ch] () {
		m[1] = v.v
		ch <- true
	}()
	v.v.y[30] = new_int()
	_ = <-ch
}

fn test_race_map_big_val_access1() {
	m := map[int]BigVal{}
	mut v := &Cell[BigVal]{}
	ch := chan bool{cap: 1}
	spawn fn [m, mut v, ch] () {
		v.v = unsafe { m[1] }
		ch <- true
	}()
	v.v.y[30] = new_int()
	_ = <-ch
}

fn test_race_map_big_val_access2() {
	m := map[int]BigVal{}
	mut v := &Cell[BigVal]{}
	ch := chan bool{cap: 1}
	spawn fn [m, mut v, ch] () {
		v.v = m[1] or { BigVal{} }
		ch <- true
	}()
	v.v.y[30] = new_int()
	_ = <-ch
}
