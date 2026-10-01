// Translated from Go's src/runtime/race/testdata/mop_test.go (part 3: TestRaceFuncCall .. end), see ../README.md.
import arrays
import hash.crc32
import io
import os
import sync
import time

struct Cell[T] {
mut:
	v T
}

fn main() {
	run('test_race_func_call', test_race_func_call)
	run('test_race_method_call', test_race_method_call)
	run('test_race_method_call2', test_race_method_call2)
	run('test_race_method_value', test_race_method_value)
	run('test_race_method_value2', test_race_method_value2)
	run('test_race_method_value3', test_race_method_value3)
	run('test_no_race_method_value', test_no_race_method_value)
	run('test_race_panic_arg', test_race_panic_arg)
	run('test_race_defer_arg', test_race_defer_arg)
	run('test_race_defer_arg2', test_race_defer_arg2)
	run('test_no_race_addr_expr', test_no_race_addr_expr)
	run('test_race_addr_expr', test_race_addr_expr)
	run('test_race_type_assert', test_race_type_assert)
	run('test_race_block_as', test_race_block_as)
	run('test_race_block_call1', test_race_block_call1)
	run('test_race_block_call2', test_race_block_call2)
	run('test_race_block_call3', test_race_block_call3)
	run('test_race_block_call4', test_race_block_call4)
	run('test_race_block_call5', test_race_block_call5)
	run('test_race_block_call6', test_race_block_call6)
	run('test_race_slice_slice', test_race_slice_slice)
	run('test_race_slice_slice2', test_race_slice_slice2)
	run('test_race_slice_string', test_race_slice_string)
	run('test_race_slice_struct', test_race_slice_struct)
	run('test_race_append_slice_struct', test_race_append_slice_struct)
	run('test_race_struct_ind', test_race_struct_ind)
	run('test_race_as_func1', test_race_as_func1)
	run('test_race_as_func2', test_race_as_func2)
	run('test_race_as_func3', test_race_as_func3)
	run('test_no_race_as_func4', test_no_race_as_func4)
	run('test_race_heap_param', test_race_heap_param)
	run('test_no_race_empty_struct', test_no_race_empty_struct)
	run('test_race_nested_struct', test_race_nested_struct)
	run('test_race_issue5567', test_race_issue5567)
	run('test_race_issue51618', test_race_issue51618)
	run('test_race_issue5654', test_race_issue5654)
	run('test_no_race_method_thunk', test_no_race_method_thunk)
	run('test_race_method_thunk', test_race_method_thunk)
	run('test_race_method_thunk2', test_race_method_thunk2)
	run('test_race_method_thunk3', test_race_method_thunk3)
	run('test_race_method_thunk4', test_race_method_thunk4)
	run('test_no_race_tiny_alloc', test_no_race_tiny_alloc)
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

interface Inter {
	foo(x int)
}

struct InterImpl {
	x int
	y int
}

@[noinline]
fn (p InterImpl) foo(x int) {
}

// InterImpl2 is Go's `type InterImpl2 InterImpl`: a distinct type with the fields of
// InterImpl, whose foo method has a pointer receiver.
struct InterImpl2 {
	x int
	y int
}

fn (p &InterImpl2) foo(x int) {
	if isnil(p) {
		InterImpl{}.foo(x)
	}
	InterImpl{
		x: p.x
		y: p.y
	}.foo(x)
}

fn test_race_func_call() {
	c := chan bool{cap: 1}
	f := fn (x int, y int) {
		_ = y
	}
	x := 0
	mut y := &Cell[int]{}
	spawn fn [mut y, c] () {
		y.v = 42
		c <- true
	}()
	f(x, y.v)
	_ = <-c
}

fn test_race_method_call() {
	c := chan bool{cap: 1}
	i := InterImpl{}
	mut x := &Cell[int]{}
	spawn fn [mut x, c] () {
		x.v = 42
		c <- true
	}()
	i.foo(x.v)
	_ = <-c
}

fn test_race_method_call2() {
	c := chan bool{cap: 1}
	mut i := &Cell[&InterImpl]{
		v: &InterImpl{}
	}
	spawn fn [mut i, c] () {
		i.v = &InterImpl{}
		c <- true
	}()
	i.v.foo(0)
	_ = <-c
}

// Method value with concrete value receiver.
fn test_race_method_value() {
	c := chan bool{cap: 1}
	mut i := &Cell[InterImpl]{
		v: InterImpl{}
	}
	spawn fn [mut i, c] () {
		i.v = InterImpl{}
		c <- true
	}()
	_ = i.v.foo
	_ = <-c
}

// Method value with interface receiver.
fn test_race_method_value2() {
	c := chan bool{cap: 1}
	mut i := &Cell[Inter]{
		v: InterImpl{}
	}
	spawn fn [mut i, c] () {
		i.v = InterImpl{}
		c <- true
	}()
	_ = i.v.foo
	_ = <-c
}

// Method value with implicit dereference.
fn test_race_method_value3() {
	c := chan bool{cap: 1}
	mut i := &InterImpl{}
	spawn fn [mut i, c] () {
		unsafe {
			*i = InterImpl{}
		}
		c <- true
	}()
	_ = i.foo // dereferences i.
	_ = <-c
}

// Method value implicitly taking receiver address.
fn test_no_race_method_value() {
	c := chan bool{cap: 1}
	mut i := &Cell[InterImpl2]{
		v: InterImpl2{}
	}
	spawn fn [mut i, c] () {
		i.v = InterImpl2{}
		c <- true
	}()
	// V allows a method value with a reference receiver only in `unsafe` code.
	unsafe {
		_ = i.v.foo // takes the address of i only.
	}
	_ = <-c
}

fn test_race_panic_arg() {
	c := chan bool{cap: 1}
	mut err := &Cell[IError]{
		v: error('err')
	}
	spawn fn [mut err, c] () {
		err.v = error('err2')
		c <- true
	}()
	defer {
		_ = recover()
		_ = <-c
	}
	panic(err.v)
}

fn test_race_defer_arg() {
	c := chan bool{cap: 1}
	mut x := &Cell[int]{}
	spawn fn [mut x, c] () {
		x.v = 42
		c <- true
	}()
	fn [x] () {
		defer {
			fn (x int) {
			}(x.v)
		}
	}()
	_ = <-c
}

type DeferT = int

fn (d DeferT) foo() {
}

fn test_race_defer_arg2() {
	c := chan bool{cap: 1}
	mut x := &Cell[DeferT]{}
	spawn fn [mut x, c] () {
		y := DeferT(0)
		x.v = y
		c <- true
	}()
	fn [x] () {
		defer {
			x.v.foo()
		}
	}()
	_ = <-c
}

fn test_no_race_addr_expr() {
	c := chan bool{cap: 1}
	mut x := &Cell[int]{}
	spawn fn [mut x, c] () {
		x.v = 42
		c <- true
	}()
	_ = &x.v
	_ = <-c
}

struct AddrT {
	pad [256]u8
	x   int
}

struct AddrT2 {
	pad [512]u8
mut:
	p &AddrT
}

fn test_race_addr_expr() {
	c := chan bool{cap: 1}
	mut a := &AddrT2{
		p: &AddrT{
			x: 42
		}
	}
	spawn fn [mut a, c] () {
		a.p = &AddrT{
			x: 43
		}
		c <- true
	}()
	_ = &a.p.x
	_ = <-c
}

// Any is Go's `any`.
interface Any {}

fn test_race_type_assert() {
	c := chan bool{cap: 1}
	x := 0
	mut i := &Cell[Any]{
		v: x
	}
	spawn fn [mut i, c] () {
		y := 0
		i.v = y
		c <- true
	}()
	_ = i.v as int
	_ = <-c
}

fn test_race_block_as() {
	c := chan bool{cap: 1}
	mut x := &Cell[int]{}
	mut y := 0
	spawn fn [mut x, c] () {
		x.v = 42
		c <- true
	}()
	x.v, y = y, x.v
	_ = <-c
}

fn test_race_block_call1() {
	done := chan bool{}
	mut x := &Cell[int]{}
	mut y := &Cell[int]{}
	spawn fn [mut x, mut y, done] () {
		f := fn () (int, int) {
			return 42, 43
		}
		x.v, y.v = f()
		done <- true
	}()
	_ = x.v
	_ = <-done
	if x.v != 42 || y.v != 43 {
		panic('corrupted data')
	}
}

fn test_race_block_call2() {
	done := chan bool{}
	mut x := &Cell[int]{}
	mut y := &Cell[int]{}
	spawn fn [mut x, mut y, done] () {
		f := fn () (int, int) {
			return 42, 43
		}
		x.v, y.v = f()
		done <- true
	}()
	_ = y.v
	_ = <-done
	if x.v != 42 || y.v != 43 {
		panic('corrupted data')
	}
}

fn test_race_block_call3() {
	done := chan bool{}
	mut x := &Cell[&int]{
		v: unsafe { nil }
	}
	mut y := &Cell[int]{}
	spawn fn [mut x, mut y, done] () {
		f := fn () (&int, int) {
			i := 42
			return &i, 43
		}
		x.v, y.v = f()
		done <- true
	}()
	_ = x.v
	_ = <-done
	if *x.v != 42 || y.v != 43 {
		panic('corrupted data')
	}
}

fn test_race_block_call4() {
	done := chan bool{}
	mut x := &Cell[int]{}
	mut y := &Cell[&int]{
		v: unsafe { nil }
	}
	spawn fn [mut x, mut y, done] () {
		f := fn () (int, &int) {
			i := 43
			return 42, &i
		}
		x.v, y.v = f()
		done <- true
	}()
	_ = y.v
	_ = <-done
	if x.v != 42 || *y.v != 43 {
		panic('corrupted data')
	}
}

fn test_race_block_call5() {
	done := chan bool{}
	mut x := &Cell[&int]{
		v: unsafe { nil }
	}
	mut y := &Cell[int]{}
	spawn fn [mut x, mut y, done] () {
		f := fn () (&int, int) {
			i := 42
			return &i, 43
		}
		x.v, y.v = f()
		done <- true
	}()
	_ = y.v
	_ = <-done
	if *x.v != 42 || y.v != 43 {
		panic('corrupted data')
	}
}

fn test_race_block_call6() {
	done := chan bool{}
	mut x := &Cell[int]{}
	mut y := &Cell[&int]{
		v: unsafe { nil }
	}
	spawn fn [mut x, mut y, done] () {
		f := fn () (int, &int) {
			i := 43
			return 42, &i
		}
		x.v, y.v = f()
		done <- true
	}()
	_ = x.v
	_ = <-done
	if x.v != 42 || *y.v != 43 {
		panic('corrupted data')
	}
}

fn test_race_slice_slice() {
	c := chan bool{cap: 1}
	mut x := &Cell[[]int]{
		v: []int{len: 10}
	}
	spawn fn [mut x, c] () {
		x.v = []int{len: 20}
		c <- true
	}()
	_ = x.v[2..3]
	_ = <-c
}

fn test_race_slice_slice2() {
	c := chan bool{cap: 1}
	x := []int{len: 10}
	mut i := &Cell[int]{
		v: 2
	}
	spawn fn [mut i, c] () {
		i.v = 3
		c <- true
	}()
	_ = x[i.v..4]
	_ = <-c
}

fn test_race_slice_string() {
	c := chan bool{cap: 1}
	mut x := &Cell[string]{
		v: 'hello'
	}
	spawn fn [mut x, c] () {
		x.v = 'world'
		c <- true
	}()
	_ = x.v[2..3]
	_ = <-c
}

// SliceStructX is the local type X of Go's TestRaceSliceStruct.
struct SliceStructX {
mut:
	x int
	y int
}

fn test_race_slice_struct() {
	c := chan bool{cap: 1}
	mut x := &Cell[[]SliceStructX]{
		v: []SliceStructX{len: 10}
	}
	spawn fn [x, c] () {
		mut y := []SliceStructX{len: 10}
		arrays.copy(mut y, x.v)
		c <- true
	}()
	x.v[1].y = 42
	_ = <-c
}

// AppendSliceStructX is the local type X of Go's TestRaceAppendSliceStruct.
struct AppendSliceStructX {
mut:
	x int
	y int
}

fn test_race_append_slice_struct() {
	c := chan bool{cap: 1}
	mut x := &Cell[[]AppendSliceStructX]{
		v: []AppendSliceStructX{len: 10}
	}
	spawn fn [x, c] () {
		mut y := []AppendSliceStructX{cap: 10}
		y << x.v
		c <- true
	}()
	x.v[1].y = 42
	_ = <-c
}

// StructIndItem is the local type Item of Go's TestRaceStructInd.
struct StructIndItem {
mut:
	x int
	y int
}

fn test_race_struct_ind() {
	c := chan bool{cap: 1}
	mut i := StructIndItem{}
	spawn fn (mut p StructIndItem, c chan bool) {
		p = StructIndItem{}
		c <- true
	}(mut i, c)
	i.y = 42
	_ = <-c
}

fn test_race_as_func1() {
	mut s := &Cell[[]u8]{}
	c := chan bool{cap: 1}
	spawn fn [mut s, c] () {
		mut err := ?IError(none)
		s.v, err = fn () ([]u8, ?IError) {
			t := 'hello world'.bytes()
			return t, none
		}()
		c <- true
		_ = err
	}()
	_ = s.v.bytestr()
	_ = <-c
}

fn test_race_as_func2() {
	c := chan bool{cap: 1}
	mut x := &Cell[int]{}
	spawn fn [x, c] () {
		fn (x int) {
			_ = x
		}(x.v)
		c <- true
	}()
	x.v = 42
	_ = <-c
}

fn test_race_as_func3() {
	c := chan bool{cap: 1}
	mut mu := sync.new_mutex()
	mut x := &Cell[int]{}
	spawn fn [mut mu, x, c] () {
		// The race only happens when the main thread takes the mutex first. Go's harness
		// gets that order from GOMAXPROCS=1, where this goroutine only starts once the test
		// blocks; a sleep is no synchronization for the race detector.
		time.sleep(5 * time.millisecond)
		fn [mut mu] (x int) {
			_ = x
			mu.lock()
		}(x.v) // Read of x must be outside of the mutex.
		mu.unlock()
		c <- true
	}()
	mu.lock()
	x.v = 42
	mu.unlock()
	_ = <-c
}

fn test_no_race_as_func4() {
	c := chan bool{cap: 1}
	mut mu := sync.new_mutex()
	mut x := &Cell[int]{}
	_ = x
	spawn fn [mut mu, mut x, c] () {
		x.v = fn [mut mu] () int { // Write of x must be under the mutex.
			mu.lock()
			return 42
		}()
		mu.unlock()
		c <- true
	}()
	mu.lock()
	x.v = 42
	mu.unlock()
	_ = <-c
}

fn test_race_heap_param() {
	done := chan bool{}
	x := fn [done] () int {
		// Go's named result x, which the goroutine writes.
		mut x := &Cell[int]{}
		spawn fn [mut x, done] () {
			x.v = 42
			done <- true
		}()
		return x.v
	}()
	_ = x
	_ = <-done
}

// EmptyStructEmpty, EmptyStructX and EmptyStructY are the local types Empty, X and Y of
// Go's TestNoRaceEmptyStruct. V only allows embedded structs at the beginning of a struct,
// so EmptyStructX has a named field for Empty, which keeps Go's layout (Empty last).
struct EmptyStructEmpty {}

struct EmptyStructX {
	y     i64
	empty EmptyStructEmpty
}

struct EmptyStructY {
mut:
	x EmptyStructX
	y i64
}

fn test_no_race_empty_struct() {
	c := chan EmptyStructX{}
	mut y := &EmptyStructY{}
	spawn fn [y, c] () {
		x := y.x
		c <- x
	}()
	y.y = 42
	_ = <-c
}

// NestedStructX and NestedStructY are the local types X and Y of Go's
// TestRaceNestedStruct.
struct NestedStructX {
mut:
	x int
	y int
}

struct NestedStructY {
mut:
	x NestedStructX
}

fn test_race_nested_struct() {
	c := chan NestedStructY{}
	mut y := &NestedStructY{}
	spawn fn [y, c] () {
		c <- *y
	}()
	y.x.y = 42
	_ = <-c
}

fn test_race_issue5567() {
	race_read(false)
}

fn test_race_issue51618() {
	race_read(true)
}

// go_error returns what Go's err.Error() returns for the error. The tests below represent
// Go's `error` values by that string, '' is `nil`.
fn go_error(err IError) string {
	if err is os.Eof || err is io.Eof {
		return 'EOF'
	}
	return err.msg()
}

// race_read is Go's testRaceRead. Go reads its own source file mop_test.go, this reads
// the V translation.
fn race_read(pread bool) {
	in_ := chan []u8{}
	res := chan string{}
	spawn fn [pread, in_, res] () {
		mut err_msg := ''
		defer {
			in_.close()
			res <- err_msg
		}
		path := @FILE
		mut f := os.open(path) or {
			err_msg = go_error(err)
			return
		}
		defer {
			f.close()
		}
		mut n := 0
		mut total := 0
		mut b := []u8{len: 17} // the race is on b buffer
		for err_msg == '' {
			if pread {
				n = f.read_from(u64(total), mut b) or {
					err_msg = go_error(err)
					0
				}
			} else {
				n = f.read(mut b) or {
					err_msg = go_error(err)
					0
				}
			}
			total += n
			if n > 0 {
				in_ <- b[..n]
			}
		}
		if err_msg == 'EOF' {
			err_msg = ''
		}
	}()
	h := crc32.new(0x12345678)
	mut crc := u32(0)
	for {
		b := <-in_ or { break }
		crc = h.update(crc, b)
	}
	_ = crc
	err_msg := <-res
	if err_msg != '' {
		panic(err_msg)
	}
}

// Buffer is the part of Go's bytes.Buffer that TestRaceIssue5654 uses.
struct Buffer {
mut:
	buf []u8
	off int
}

fn new_buffer_string(s string) &Buffer {
	return &Buffer{
		buf: s.bytes()
	}
}

// read reads the next p.len bytes from the buffer or until the buffer is drained, like
// Go's Buffer.Read.
fn (mut b Buffer) read(mut p []u8) !int {
	if b.off >= b.buf.len {
		b.buf = b.buf[..0]
		b.off = 0
		if p.len == 0 {
			return 0
		}
		return io.Eof{}
	}
	n := copy(mut p, b.buf[b.off..])
	b.off += n
	return n
}

fn test_race_issue5654() {
	text := "Friends, Romans, countrymen, lend me your ears;
I come to bury Caesar, not to praise him.
The evil that men do lives after them;
The good is oft interred with their bones;
So let it be with Caesar. The noble Brutus
Hath told you Caesar was ambitious:
If it were so, it was a grievous fault,
And grievously hath Caesar answer'd it.
Here, under leave of Brutus and the rest -
For Brutus is an honourable man;
So are they all, all honourable men -
Come I to speak in Caesar's funeral.
He was my friend, faithful and just to me:
But Brutus says he was ambitious;
And Brutus is an honourable man."

	mut data := new_buffer_string(text)
	in_ := chan []u8{}

	spawn fn [mut data, in_] () {
		mut buf := []u8{len: 16}
		mut n := 0
		mut err_msg := ''
		// Go: `for ; err == nil; n, err = data.Read(buf) { in <- buf[:n] }`
		for err_msg == '' {
			in_ <- buf[..n]
			n = data.read(mut buf) or {
				err_msg = go_error(err)
				0
			}
		}
		in_.close()
	}()
	mut res := ''
	for {
		s := <-in_ or { break }
		res += s.bytestr()
	}
	_ = res
}

// Base is Go's `type Base int`. V can only embed structs, so it is a struct with one int.
struct Base {
mut:
	v int
}

fn (b &Base) foo() int {
	return 42
}

fn (b Base) bar() int {
	return b.v
}

// NoRaceMethodThunkDerived is the local type Derived of Go's TestNoRaceMethodThunk. V only
// allows embedded structs at the beginning of a struct, so Base comes before pad.
struct NoRaceMethodThunkDerived {
	Base
	pad int
}

fn test_no_race_method_thunk() {
	mut d := &Cell[NoRaceMethodThunkDerived]{}
	done := chan bool{}
	spawn fn [d, done] () {
		_ = d.v.foo()
		done <- true
	}()
	d.v = NoRaceMethodThunkDerived{}
	_ = <-done
}

// RaceMethodThunkDerived is the local type Derived of Go's TestRaceMethodThunk,
// TestRaceMethodThunk3 and TestRaceMethodThunk4, which embeds a *Base. V cannot embed a
// pointer, so it is a named field, and the promoted methods are called through it.
struct RaceMethodThunkDerived {
	pad int
mut:
	base &Base = unsafe { nil }
}

fn test_race_method_thunk() {
	mut d := &Cell[RaceMethodThunkDerived]{}
	done := chan bool{}
	spawn fn [d, done] () {
		_ = d.v.base.foo()
		done <- true
	}()
	d.v = RaceMethodThunkDerived{}
	_ = <-done
}

// RaceMethodThunk2Derived is the local type Derived of Go's TestRaceMethodThunk2 (Base
// first, as above).
struct RaceMethodThunk2Derived {
	Base
	pad int
}

fn test_race_method_thunk2() {
	mut d := &Cell[RaceMethodThunk2Derived]{}
	done := chan bool{}
	spawn fn [d, done] () {
		_ = d.v.bar()
		done <- true
	}()
	d.v = RaceMethodThunk2Derived{}
	_ = <-done
}

fn test_race_method_thunk3() {
	mut d := &Cell[RaceMethodThunkDerived]{}
	d.v.base = &Base{}
	done := chan bool{}
	spawn fn [d, done] () {
		_ = d.v.base.bar()
		done <- true
	}()
	d.v.base = &Base{}
	_ = <-done
}

fn test_race_method_thunk4() {
	mut d := &Cell[RaceMethodThunkDerived]{}
	d.v.base = &Base{}
	done := chan bool{}
	spawn fn [d, done] () {
		_ = d.v.base.bar()
		done <- true
	}()
	d.v.base.v = 42
	_ = <-done
}

const tiny_alloc_p = 4
const tiny_alloc_n = 1_000_000

fn test_no_race_tiny_alloc() {
	mut tiny_sink := &Cell[&u8]{
		v: unsafe { nil }
	}
	_ = tiny_sink
	done := chan bool{}
	for p := 0; p < tiny_alloc_p; p++ {
		spawn fn [mut tiny_sink, done] () {
			for i := 0; i < tiny_alloc_n; i++ {
				mut b := u8(0)
				if b != 0 {
					tiny_sink.v = &b // make it heap allocated
				}
				b = 42
			}
			done <- true
		}()
	}
	for p := 0; p < tiny_alloc_p; p++ {
		_ = <-done
	}
}

// Go's TestNoRaceIssue60934 is not translated: it checks that the race-ignore state that
// runtime.RaceDisable() leaves in finished goroutines is not inherited by new goroutines
// that reuse their race contexts. V has no counterpart of runtime.RaceDisable, and V
// threads are OS threads, each with its own ThreadSanitizer state.
