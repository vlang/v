// Translated from Go's src/runtime/race/testdata/atomic_test.go, see ../README.md.
import sync.stdatomic
import time

struct Cell[T] {
mut:
	v T
}

fn main() {
	run('test_no_race_atomic_add_int64', test_no_race_atomic_add_int64)
	run('test_race_atomic_add_int64', test_race_atomic_add_int64)
	run('test_no_race_atomic_add_int32', test_no_race_atomic_add_int32)
	run('test_no_race_atomic_load_add_int32', test_no_race_atomic_load_add_int32)
	run('test_no_race_atomic_load_store_int32', test_no_race_atomic_load_store_int32)
	run('test_no_race_atomic_store_cas_int32', test_no_race_atomic_store_cas_int32)
	run('test_no_race_atomic_cas_load_int32', test_no_race_atomic_cas_load_int32)
	run('test_no_race_atomic_cas_cas_int32', test_no_race_atomic_cas_cas_int32)
	run('test_no_race_atomic_cas_cas_int32_2', test_no_race_atomic_cas_cas_int32_2)
	run('test_no_race_atomic_load_int64', test_no_race_atomic_load_int64)
	run('test_no_race_atomic_cas_cas_uint64', test_no_race_atomic_cas_cas_uint64)
	run('test_no_race_atomic_load_store_pointer', test_no_race_atomic_load_store_pointer)
	run('test_no_race_atomic_store_cas_uint64', test_no_race_atomic_store_cas_uint64)
	run('test_race_atomic_store_load', test_race_atomic_store_load)
	run('test_race_atomic_load_store', test_race_atomic_load_store)
	run('test_race_atomic_add_load', test_race_atomic_add_load)
	run('test_race_atomic_add_store', test_race_atomic_add_store)
	run('test_no_race_defer_atomic_store', test_no_race_defer_atomic_store)
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

// The Go sync/atomic functions that vlib/sync/stdatomic has no function for.

fn atomic_add_i32(addr &i32, delta i32) i32 {
	return i32(C.atomic_fetch_add_u32(voidptr(addr), u32(delta))) + delta
}

fn atomic_load_i32(addr &i32) i32 {
	return i32(C.atomic_load_u32(voidptr(addr)))
}

fn atomic_store_i32(addr &i32, val i32) {
	C.atomic_store_u32(voidptr(addr), u32(val))
}

fn atomic_compare_and_swap_i32(addr &i32, old i32, new_val i32) bool {
	mut expected := u32(old)
	return C.atomic_compare_exchange_strong_u32(voidptr(addr), &expected, u32(new_val))
}

fn atomic_compare_and_swap_u64(addr &u64, old u64, new_val u64) bool {
	mut expected := old
	return C.atomic_compare_exchange_strong_u64(voidptr(addr), &expected, new_val)
}

fn atomic_load_pointer(addr &voidptr) voidptr {
	return C.atomic_load_ptr(voidptr(addr))
}

fn atomic_store_pointer(addr &voidptr, val voidptr) {
	C.atomic_store_ptr(voidptr(addr), val)
}

fn test_no_race_atomic_add_int64() {
	mut x1 := &Cell[i8]{}
	mut x2 := &Cell[i8]{}
	_ = x1.v + x2.v
	mut s := &Cell[i64]{}
	ch := chan bool{cap: 2}
	spawn fn [mut x1, mut x2, mut s, ch] () {
		x1.v = 1
		if stdatomic.add_i64(&s.v, 1) == 2 {
			x2.v = 1
		}
		ch <- true
	}()
	spawn fn [mut x1, mut x2, mut s, ch] () {
		x2.v = 1
		if stdatomic.add_i64(&s.v, 1) == 2 {
			x1.v = 1
		}
		ch <- true
	}()
	_ = <-ch
	_ = <-ch
}

fn test_race_atomic_add_int64() {
	mut x1 := &Cell[i8]{}
	mut x2 := &Cell[i8]{}
	_ = x1.v + x2.v
	mut s := &Cell[i64]{}
	ch := chan bool{cap: 2}
	spawn fn [mut x1, mut x2, mut s, ch] () {
		x1.v = 1
		if stdatomic.add_i64(&s.v, 1) == 1 {
			x2.v = 1
		}
		ch <- true
	}()
	spawn fn [mut x1, mut x2, mut s, ch] () {
		x2.v = 1
		if stdatomic.add_i64(&s.v, 1) == 1 {
			x1.v = 1
		}
		ch <- true
	}()
	_ = <-ch
	_ = <-ch
}

fn test_no_race_atomic_add_int32() {
	mut x1 := &Cell[i8]{}
	mut x2 := &Cell[i8]{}
	_ = x1.v + x2.v
	mut s := &Cell[i32]{}
	ch := chan bool{cap: 2}
	spawn fn [mut x1, mut x2, mut s, ch] () {
		x1.v = 1
		if atomic_add_i32(&s.v, 1) == 2 {
			x2.v = 1
		}
		ch <- true
	}()
	spawn fn [mut x1, mut x2, mut s, ch] () {
		x2.v = 1
		if atomic_add_i32(&s.v, 1) == 2 {
			x1.v = 1
		}
		ch <- true
	}()
	_ = <-ch
	_ = <-ch
}

fn test_no_race_atomic_load_add_int32() {
	mut x := &Cell[i64]{}
	_ = x.v
	mut s := &Cell[i32]{}
	spawn fn [mut x, mut s] () {
		x.v = 2
		atomic_add_i32(&s.v, 1)
	}()
	for atomic_load_i32(&s.v) != 1 {
		time.sleep(0)
	}
	x.v = 1
}

fn test_no_race_atomic_load_store_int32() {
	mut x := &Cell[i64]{}
	_ = x.v
	mut s := &Cell[i32]{}
	spawn fn [mut x, mut s] () {
		x.v = 2
		atomic_store_i32(&s.v, 1)
	}()
	for atomic_load_i32(&s.v) != 1 {
		time.sleep(0)
	}
	x.v = 1
}

fn test_no_race_atomic_store_cas_int32() {
	mut x := &Cell[i64]{}
	_ = x.v
	mut s := &Cell[i32]{}
	spawn fn [mut x, mut s] () {
		x.v = 2
		atomic_store_i32(&s.v, 1)
	}()
	for !atomic_compare_and_swap_i32(&s.v, 1, 0) {
		time.sleep(0)
	}
	x.v = 1
}

fn test_no_race_atomic_cas_load_int32() {
	mut x := &Cell[i64]{}
	_ = x.v
	mut s := &Cell[i32]{}
	spawn fn [mut x, mut s] () {
		x.v = 2
		if !atomic_compare_and_swap_i32(&s.v, 0, 1) {
			panic('')
		}
	}()
	for atomic_load_i32(&s.v) != 1 {
		time.sleep(0)
	}
	x.v = 1
}

fn test_no_race_atomic_cas_cas_int32() {
	mut x := &Cell[i64]{}
	_ = x.v
	mut s := &Cell[i32]{}
	spawn fn [mut x, mut s] () {
		x.v = 2
		if !atomic_compare_and_swap_i32(&s.v, 0, 1) {
			panic('')
		}
	}()
	for !atomic_compare_and_swap_i32(&s.v, 1, 0) {
		time.sleep(0)
	}
	x.v = 1
}

fn test_no_race_atomic_cas_cas_int32_2() {
	mut x1 := &Cell[i8]{}
	mut x2 := &Cell[i8]{}
	_ = x1.v + x2.v
	mut s := &Cell[i32]{}
	ch := chan bool{cap: 2}
	spawn fn [mut x1, mut x2, mut s, ch] () {
		x1.v = 1
		if !atomic_compare_and_swap_i32(&s.v, 0, 1) {
			x2.v = 1
		}
		ch <- true
	}()
	spawn fn [mut x1, mut x2, mut s, ch] () {
		x2.v = 1
		if !atomic_compare_and_swap_i32(&s.v, 0, 1) {
			x1.v = 1
		}
		ch <- true
	}()
	_ = <-ch
	_ = <-ch
}

fn test_no_race_atomic_load_int64() {
	mut x := &Cell[i32]{}
	_ = x.v
	mut s := &Cell[i64]{}
	spawn fn [mut x, mut s] () {
		x.v = 2
		stdatomic.add_i64(&s.v, 1)
	}()
	for stdatomic.load_i64(&s.v) != 1 {
		time.sleep(0)
	}
	x.v = 1
}

fn test_no_race_atomic_cas_cas_uint64() {
	mut x := &Cell[i64]{}
	_ = x.v
	mut s := &Cell[u64]{}
	spawn fn [mut x, mut s] () {
		x.v = 2
		if !atomic_compare_and_swap_u64(&s.v, 0, 1) {
			panic('')
		}
	}()
	for !atomic_compare_and_swap_u64(&s.v, 1, 0) {
		time.sleep(0)
	}
	x.v = 1
}

fn test_no_race_atomic_load_store_pointer() {
	mut x := &Cell[i64]{}
	_ = x.v
	mut s := &Cell[voidptr]{}
	y := 2
	p := voidptr(&y)
	spawn fn [mut x, mut s, p] () {
		x.v = 2
		atomic_store_pointer(&s.v, p)
	}()
	for atomic_load_pointer(&s.v) != p {
		time.sleep(0)
	}
	x.v = 1
}

fn test_no_race_atomic_store_cas_uint64() {
	mut x := &Cell[i64]{}
	_ = x.v
	mut s := &Cell[u64]{}
	spawn fn [mut x, mut s] () {
		x.v = 2
		stdatomic.store_u64(&s.v, 1)
	}()
	for !atomic_compare_and_swap_u64(&s.v, 1, 0) {
		time.sleep(0)
	}
	x.v = 1
}

fn test_race_atomic_store_load() {
	c := chan bool{}
	mut a := &Cell[u64]{}
	spawn fn [mut a, c] () {
		stdatomic.store_u64(&a.v, 1)
		c <- true
	}()
	_ = a.v
	_ = <-c
}

fn test_race_atomic_load_store() {
	c := chan bool{}
	mut a := &Cell[u64]{}
	spawn fn [mut a, c] () {
		_ = stdatomic.load_u64(&a.v)
		c <- true
	}()
	a.v = 1
	_ = <-c
}

fn test_race_atomic_add_load() {
	c := chan bool{}
	mut a := &Cell[u64]{}
	spawn fn [mut a, c] () {
		stdatomic.add_u64(&a.v, 1)
		c <- true
	}()
	_ = a.v
	_ = <-c
}

fn test_race_atomic_add_store() {
	c := chan bool{}
	mut a := &Cell[u64]{}
	spawn fn [mut a, c] () {
		stdatomic.add_u64(&a.v, 1)
		c <- true
	}()
	a.v = 42
	_ = <-c
}

// Go's TestNoRaceAtomicCrash is not translated: it checks that an atomic operation on a nil
// pointer panics without deadlocking the program, and recovers from that panic; V has no
// recover, and a nil pointer dereference is a fatal signal.

// Foo is the `foo` type that Go's TestNoRaceDeferAtomicStore declares inside the test.
struct Foo {
mut:
	bar i64
}

// do_fork is the recursive `doFork` closure of Go's test (a V closure cannot call itself).
fn do_fork(mut f Foo, depth int) {
	stdatomic.store_i64(&f.bar, 1)
	defer {
		stdatomic.store_i64(&f.bar, 0)
	}
	if depth > 0 {
		for i := 0; i < 2; i++ {
			mut f2 := &Foo{}
			spawn do_fork(mut f2, depth - 1)
		}
	}
}

fn test_no_race_defer_atomic_store() {
	// Test that when an atomic function is deferred directly, the
	// GC scans it correctly. See issue 42599.
	// (Go's runtime.GC() call at the end of doFork is dropped: race builds of V have no GC.)
	mut f := &Foo{}
	do_fork(mut f, 11)
}
