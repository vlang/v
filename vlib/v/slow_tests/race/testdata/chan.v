// Translated from Go's src/runtime/race/testdata/chan_test.go, see ../README.md.
import time

struct Cell[T] {
mut:
	v T
}

// Empty is Go's `struct{}`.
struct Empty {}

// Msg is the `msg` type of Go's TestNoRaceChanPtr.
struct Msg {
mut:
	x int
}

// Task is the `Task` type of Go's TestNoRaceProducerConsumerUnbuffered.
struct Task {
	f    fn () @[required]
	done chan bool
}

fn main() {
	run('test_no_race_chan_sync', test_no_race_chan_sync)
	run('test_no_race_chan_sync_rev', test_no_race_chan_sync_rev)
	run('test_no_race_chan_async', test_no_race_chan_async)
	run('test_race_chan_async_rev', test_race_chan_async_rev)
	run('test_no_race_chan_async_close_recv', test_no_race_chan_async_close_recv)
	run('test_no_race_chan_async_close_recv2', test_no_race_chan_async_close_recv2)
	run('test_no_race_chan_async_close_recv3', test_no_race_chan_async_close_recv3)
	run('test_no_race_chan_sync_close_recv', test_no_race_chan_sync_close_recv)
	run('test_no_race_chan_sync_close_recv2', test_no_race_chan_sync_close_recv2)
	run('test_no_race_chan_sync_close_recv3', test_no_race_chan_sync_close_recv3)
	run('test_race_chan_sync_close_send', test_race_chan_sync_close_send)
	run('test_race_chan_async_close_send', test_race_chan_async_close_send)
	run('test_race_chan_close_close', test_race_chan_close_close)
	run('test_race_chan_send_len', test_race_chan_send_len)
	run('test_race_chan_recv_len', test_race_chan_recv_len)
	run('test_race_chan_send_send', test_race_chan_send_send)
	run('test_no_race_chan_ptr', test_no_race_chan_ptr)
	run('test_race_chan_wrong_send', test_race_chan_wrong_send)
	run('test_race_chan_wrong_close', test_race_chan_wrong_close)
	run('test_race_chan_send_close', test_race_chan_send_close)
	run('test_race_chan_send_select_close', test_race_chan_send_select_close)
	run('test_race_select_read_write_async', test_race_select_read_write_async)
	run('test_race_select_read_write_sync', test_race_select_read_write_sync)
	run('test_no_race_select_read_write_async', test_no_race_select_read_write_async)
	run('test_race_chan_read_write_async', test_race_chan_read_write_async)
	run('test_race_chan_read_write_sync', test_race_chan_read_write_sync)
	run('test_no_race_chan_read_write_async', test_no_race_chan_read_write_async)
	run('test_no_race_producer_consumer_unbuffered', test_no_race_producer_consumer_unbuffered)
	run('test_race_chan_itself_send', test_race_chan_itself_send)
	run('test_race_chan_itself_recv', test_race_chan_itself_recv)
	run('test_race_chan_itself_nil', test_race_chan_itself_nil)
	run('test_race_chan_itself_close', test_race_chan_itself_close)
	run('test_race_chan_itself_len', test_race_chan_itself_len)
	run('test_race_chan_itself_cap', test_race_chan_itself_cap)
	run('test_no_race_chan_close_len', test_no_race_chan_close_len)
	run('test_no_race_chan_close_cap', test_no_race_chan_close_cap)
	run('test_race_chan_close_send', test_race_chan_close_send)
	run('test_no_race_chan_mutex', test_no_race_chan_mutex)
	run('test_no_race_select_mutex', test_no_race_select_mutex)
	run('test_race_chan_sem', test_race_chan_sem)
	run('test_no_race_chan_wait_group', test_no_race_chan_wait_group)
	run('test_no_race_blocked_send_sync', test_no_race_blocked_send_sync)
	run('test_no_race_blocked_select_send_sync', test_no_race_blocked_select_send_sync)
	run('test_no_race_close_happens_before_read', test_no_race_close_happens_before_read)
	run('test_no_race_elem_size0', test_no_race_elem_size0)
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

fn test_no_race_chan_sync() {
	mut v := &Cell[int]{}
	_ = v.v
	c := chan int{}
	spawn fn [mut v, c] () {
		v.v = 1
		c <- 0
	}()
	_ = <-c
	v.v = 2
}

fn test_no_race_chan_sync_rev() {
	mut v := &Cell[int]{}
	_ = v.v
	c := chan int{}
	spawn fn [mut v, c] () {
		c <- 0
		v.v = 2
	}()
	v.v = 1
	_ = <-c
}

fn test_no_race_chan_async() {
	mut v := &Cell[int]{}
	_ = v.v
	c := chan int{cap: 10}
	spawn fn [mut v, c] () {
		v.v = 1
		c <- 0
	}()
	_ = <-c
	v.v = 2
}

fn test_race_chan_async_rev() {
	mut v := &Cell[int]{}
	_ = v.v
	c := chan int{cap: 10}
	spawn fn [mut v, c] () {
		c <- 0
		v.v = 1
	}()
	v.v = 2
	_ = <-c
}

fn test_no_race_chan_async_close_recv() {
	mut v := &Cell[int]{}
	_ = v.v
	c := chan int{cap: 10}
	spawn fn [mut v, c] () {
		v.v = 1
		c.close()
	}()
	// Go defers `recover(); v = 2`: receiving from a closed channel does not panic, so the
	// recover() is a no-op there.
	fn [mut v, c] () {
		defer {
			v.v = 2
		}
		_ = <-c
	}()
}

fn test_no_race_chan_async_close_recv2() {
	mut v := &Cell[int]{}
	_ = v.v
	c := chan int{cap: 10}
	spawn fn [mut v, c] () {
		v.v = 1
		c.close()
	}()
	_ = <-c or { 0 }
	v.v = 2
}

fn test_no_race_chan_async_close_recv3() {
	mut v := &Cell[int]{}
	_ = v.v
	c := chan int{cap: 10}
	spawn fn [mut v, c] () {
		v.v = 1
		c.close()
	}()
	for {
		_ := <-c or { break }
	}
	v.v = 2
}

fn test_no_race_chan_sync_close_recv() {
	mut v := &Cell[int]{}
	_ = v.v
	c := chan int{}
	spawn fn [mut v, c] () {
		v.v = 1
		c.close()
	}()
	// Go defers `recover(); v = 2`: receiving from a closed channel does not panic, so the
	// recover() is a no-op there.
	fn [mut v, c] () {
		defer {
			v.v = 2
		}
		_ = <-c
	}()
}

fn test_no_race_chan_sync_close_recv2() {
	mut v := &Cell[int]{}
	_ = v.v
	c := chan int{}
	spawn fn [mut v, c] () {
		v.v = 1
		c.close()
	}()
	_ = <-c or { 0 }
	v.v = 2
}

fn test_no_race_chan_sync_close_recv3() {
	mut v := &Cell[int]{}
	_ = v.v
	c := chan int{}
	spawn fn [mut v, c] () {
		v.v = 1
		c.close()
	}()
	for {
		_ := <-c or { break }
	}
	v.v = 2
}

fn test_race_chan_sync_close_send() {
	mut v := &Cell[int]{}
	_ = v.v
	c := chan int{}
	spawn fn [mut v, c] () {
		v.v = 1
		c.close()
	}()
	// Go recovers from the panic of the send on the closed channel, V's `or` branch of a
	// send runs instead of that panic.
	c <- 0 or {}
	v.v = 2
}

fn test_race_chan_async_close_send() {
	mut v := &Cell[int]{}
	_ = v.v
	c := chan int{cap: 10}
	spawn fn [mut v, c] () {
		v.v = 1
		c.close()
	}()
	// Go recovers from the panic of the send on the closed channel, which ends the loop.
	for {
		c <- 0 or { break }
	}
	v.v = 2
}

fn test_race_chan_close_close() {
	compl := chan bool{cap: 2}
	mut v1 := &Cell[int]{}
	mut v2 := &Cell[int]{}
	_ = v1.v + v2.v
	c := chan int{}
	// Go's close() of a closed channel panics, and the deferred function writes the other
	// goroutine's variable when it recovers that panic. V's close() of a closed channel does
	// nothing, so the goroutines check `c.closed` to take the same branch.
	spawn fn [mut v1, mut v2, c, compl] () {
		v1.v = 1
		if c.closed {
			v2.v = 2
		} else {
			c.close()
		}
		compl <- true
	}()
	spawn fn [mut v1, mut v2, c, compl] () {
		v2.v = 1
		if c.closed {
			v1.v = 2
		} else {
			c.close()
		}
		compl <- true
	}()
	_ = <-compl
	_ = <-compl
}

fn test_race_chan_send_len() {
	mut v := &Cell[int]{}
	_ = v.v
	c := chan int{cap: 10}
	spawn fn [mut v, c] () {
		v.v = 1
		c <- 1
	}()
	for c.len == 0 {
		time.sleep(0)
	}
	v.v = 2
}

fn test_race_chan_recv_len() {
	mut v := &Cell[int]{}
	_ = v.v
	c := chan int{cap: 10}
	c <- 1
	spawn fn [mut v, c] () {
		v.v = 1
		_ = <-c
	}()
	for c.len != 0 {
		time.sleep(0)
	}
	v.v = 2
}

fn test_race_chan_send_send() {
	compl := chan bool{cap: 2}
	mut v1 := &Cell[int]{}
	mut v2 := &Cell[int]{}
	_ = v1.v + v2.v
	c := chan int{cap: 1}
	spawn fn [mut v1, mut v2, c, compl] () {
		v1.v = 1
		select {
			c <- 1 {
			}
			else {
				v2.v = 2
			}
		}
		compl <- true
	}()
	spawn fn [mut v1, mut v2, c, compl] () {
		v2.v = 1
		select {
			c <- 1 {
			}
			else {
				v1.v = 2
			}
		}
		compl <- true
	}()
	_ = <-compl
	_ = <-compl
}

fn test_no_race_chan_ptr() {
	c := chan &Msg{}
	spawn fn [c] () {
		c <- &Msg{1}
	}()
	mut m := <-c
	m.x = 2
}

fn test_race_chan_wrong_send() {
	mut v1 := &Cell[int]{}
	mut v2 := &Cell[int]{}
	_ = v1.v + v2.v
	c := chan int{cap: 2}
	spawn fn [mut v1, c] () {
		v1.v = 1
		c <- 1
	}()
	spawn fn [mut v2, c] () {
		v2.v = 2
		c <- 2
	}()
	time.sleep(10 * time.millisecond)
	if <-c == 1 {
		v2.v = 3
	} else {
		v1.v = 3
	}
}

fn test_race_chan_wrong_close() {
	mut v1 := &Cell[int]{}
	mut v2 := &Cell[int]{}
	_ = v1.v + v2.v
	c := chan int{cap: 1}
	done := chan bool{}
	spawn fn [mut v1, c, done] () {
		v1.v = 1
		// Go recovers from a panic of this send (on a closed channel) in a deferred function,
		// which ends the goroutine.
		c <- 1 or { return }
		done <- true
	}()
	spawn fn [mut v2, c, done] () {
		time.sleep(10 * time.millisecond)
		v2.v = 2
		c.close()
		done <- true
	}()
	time.sleep(20 * time.millisecond)
	mut who := true
	_ = <-c or {
		who = false
		0
	}
	if who {
		v2.v = 2
	} else {
		v1.v = 2
	}
	_ = <-done
	_ = <-done
}

fn test_race_chan_send_close() {
	compl := chan bool{cap: 2}
	c := chan int{cap: 1}
	spawn fn [c, compl] () {
		defer {
			compl <- true
		}
		// Go recovers from a panic of this send (on a closed channel) in the deferred function.
		c <- 1 or {}
	}()
	spawn fn [c, compl] () {
		time.sleep(10 * time.millisecond)
		c.close()
		compl <- true
	}()
	_ = <-compl
	_ = <-compl
}

fn test_race_chan_send_select_close() {
	compl := chan bool{cap: 2}
	c := chan int{cap: 1}
	c1 := chan int{}
	spawn fn [c, c1, compl] () {
		defer {
			compl <- true
		}
		time.sleep(10 * time.millisecond)
		// Go's select panics on its send case of the closed `c`, and the deferred recover()
		// ends the goroutine. V's select never selects a send to a closed channel, and would
		// wait for `c1` forever, so a timeout branch ends it instead.
		select {
			c <- 1 {
			}
			_ := <-c1 {
			}
			20 * time.millisecond {
			}
		}
	}()
	spawn fn [c, compl] () {
		c.close()
		compl <- true
	}()
	_ = <-compl
	_ = <-compl
}

fn test_race_select_read_write_async() {
	done := chan bool{}
	mut x := &Cell[int]{}
	c1 := chan int{cap: 10}
	c2 := chan int{cap: 10}
	c3 := chan int{}
	c2 <- 1
	spawn fn [x, c1, c3, done] () {
		select {
			// read of x races with...
			c1 <- x.v {
			}
			c3 <- 1 {
			}
		}
		done <- true
	}()
	select {
		// ... write to x here
		x.v = <-c2 {
		}
		c3 <- 1 {
		}
	}
	_ = <-done
}

fn test_race_select_read_write_sync() {
	done := chan bool{}
	mut x := &Cell[int]{}
	c1 := chan int{}
	c2 := chan int{}
	c3 := chan int{}
	// make c1 and c2 ready for communication
	spawn fn [c1] () {
		_ = <-c1
	}()
	spawn fn [c2] () {
		c2 <- 1
	}()
	spawn fn [x, c1, c3, done] () {
		select {
			// read of x races with...
			c1 <- x.v {
			}
			c3 <- 1 {
			}
		}
		done <- true
	}()
	select {
		// ... write to x here
		x.v = <-c2 {
		}
		c3 <- 1 {
		}
	}
	_ = <-done
}

fn test_no_race_select_read_write_async() {
	done := chan bool{}
	mut x := &Cell[int]{}
	c1 := chan int{}
	c2 := chan int{}
	spawn fn [x, c1, c2, done] () {
		select {
			// read of x does not race with...
			c1 <- x.v {
			}
			c2 <- 1 {
			}
		}
		done <- true
	}()
	select {
		// ... write to x here
		x.v = <-c1 {
		}
		c2 <- 1 {
		}
	}
	_ = <-done
}

fn test_race_chan_read_write_async() {
	done := chan bool{}
	c1 := chan int{cap: 10}
	c2 := chan int{cap: 10}
	c2 <- 10
	mut x := &Cell[int]{}
	spawn fn [x, c1, done] () {
		c1 <- x.v // read of x races with...
		done <- true
	}()
	x.v = <-c2 // ... write to x here
	_ = <-done
}

fn test_race_chan_read_write_sync() {
	done := chan bool{}
	c1 := chan int{}
	c2 := chan int{}
	// make c1 and c2 ready for communication
	spawn fn [c1] () {
		_ = <-c1
	}()
	spawn fn [c2] () {
		c2 <- 10
	}()
	mut x := &Cell[int]{}
	spawn fn [x, c1, done] () {
		c1 <- x.v // read of x races with...
		done <- true
	}()
	x.v = <-c2 // ... write to x here
	_ = <-done
}

fn test_no_race_chan_read_write_async() {
	done := chan bool{}
	c1 := chan int{cap: 10}
	mut x := &Cell[int]{}
	spawn fn [x, c1, done] () {
		c1 <- x.v // read of x does not race with...
		done <- true
	}()
	x.v = <-c1 // ... write to x here
	_ = <-done
}

fn test_no_race_producer_consumer_unbuffered() {
	queue := chan Task{}

	spawn fn [queue] () {
		t := <-queue
		t.f()
		t.done <- true
	}()

	doit := fn [queue] (f fn ()) {
		done := chan bool{cap: 1}
		queue <- Task{
			f:    f
			done: done
		}
		_ = <-done
	}

	mut x := &Cell[int]{}
	doit(fn [mut x] () {
		x.v = 1
	})
	_ = x.v
}

fn test_race_chan_itself_send() {
	compl := chan bool{cap: 1}
	mut c := &Cell[chan int]{
		v: chan int{cap: 10}
	}
	spawn fn [c, compl] () {
		c.v <- 0
		compl <- true
	}()
	c.v = chan int{cap: 20}
	_ = <-compl
}

fn test_race_chan_itself_recv() {
	compl := chan bool{cap: 1}
	mut c := &Cell[chan int]{
		v: chan int{cap: 10}
	}
	c.v <- 1
	spawn fn [c, compl] () {
		_ = <-c.v
		compl <- true
	}()
	time.sleep(10 * time.millisecond)
	c.v = chan int{cap: 20}
	_ = <-compl
}

fn test_race_chan_itself_nil() {
	mut c := &Cell[chan int]{
		v: chan int{cap: 10}
	}
	spawn fn [c] () {
		c.v <- 0
	}()
	time.sleep(10 * time.millisecond)
	// V has no nil channels: store a nil pointer in the channel variable, like Go's `c = nil`.
	unsafe {
		*(&voidptr(&c.v)) = nil
	}
	_ = c.v
}

fn test_race_chan_itself_close() {
	compl := chan bool{cap: 1}
	mut c := &Cell[chan int]{
		v: chan int{}
	}
	spawn fn [c, compl] () {
		c.v.close()
		compl <- true
	}()
	c.v = chan int{}
	_ = <-compl
}

fn test_race_chan_itself_len() {
	compl := chan bool{cap: 1}
	mut c := &Cell[chan int]{
		v: chan int{}
	}
	spawn fn [c, compl] () {
		_ = c.v.len
		compl <- true
	}()
	c.v = chan int{}
	_ = <-compl
}

fn test_race_chan_itself_cap() {
	compl := chan bool{cap: 1}
	mut c := &Cell[chan int]{
		v: chan int{}
	}
	spawn fn [c, compl] () {
		_ = c.v.cap
		compl <- true
	}()
	c.v = chan int{}
	_ = <-compl
}

fn test_no_race_chan_close_len() {
	c := chan int{cap: 10}
	r := chan int{cap: 10}
	spawn fn [c, r] () {
		r <- c.len
	}()
	spawn fn [c, r] () {
		c.close()
		r <- 0
	}()
	_ = <-r
	_ = <-r
}

fn test_no_race_chan_close_cap() {
	c := chan int{cap: 10}
	r := chan int{cap: 10}
	spawn fn [c, r] () {
		r <- c.cap
	}()
	spawn fn [c, r] () {
		c.close()
		r <- 0
	}()
	_ = <-r
	_ = <-r
}

fn test_race_chan_close_send() {
	compl := chan bool{cap: 1}
	c := chan int{cap: 10}
	spawn fn [c, compl] () {
		c.close()
		compl <- true
	}()
	// Go's harness runs with GOMAXPROCS=1, so this send runs before the close there. V's
	// threads run in parallel: the `or` branch keeps a send after the close from panicking.
	c <- 0 or {}
	_ = <-compl
}

fn test_no_race_chan_mutex() {
	done := chan Empty{}
	mtx := chan Empty{cap: 1}
	mut data := &Cell[int]{}
	_ = data.v
	spawn fn [mut data, mtx, done] () {
		mtx <- Empty{}
		data.v = 42
		_ = <-mtx
		done <- Empty{}
	}()
	mtx <- Empty{}
	data.v = 43
	_ = <-mtx
	_ = <-done
}

fn test_no_race_select_mutex() {
	done := chan Empty{}
	mtx := chan Empty{cap: 1}
	aux := chan bool{}
	mut data := &Cell[int]{}
	_ = data.v
	spawn fn [mut data, mtx, aux, done] () {
		select {
			mtx <- Empty{} {
			}
			_ := <-aux {
			}
		}
		data.v = 42
		select {
			_ := <-mtx {
			}
			_ := <-aux {
			}
		}
		done <- Empty{}
	}()
	select {
		mtx <- Empty{} {
		}
		_ := <-aux {
		}
	}
	data.v = 43
	select {
		_ := <-mtx {
		}
		_ := <-aux {
		}
	}
	_ = <-done
}

fn test_race_chan_sem() {
	done := chan Empty{}
	mtx := chan bool{cap: 2}
	mut data := &Cell[int]{}
	_ = data.v
	spawn fn [mut data, mtx, done] () {
		mtx <- true
		data.v = 42
		_ = <-mtx
		done <- Empty{}
	}()
	mtx <- true
	data.v = 43
	_ = <-mtx
	_ = <-done
}

fn test_no_race_chan_wait_group() {
	n := 10
	chan_wg := chan bool{cap: n / 2}
	mut data := []int{len: n}
	for i := 0; i < n; i++ {
		chan_wg <- true
		spawn fn [mut data, chan_wg] (i int) {
			data[i] = 42
			_ = <-chan_wg
		}(i)
	}
	for i := 0; i < chan_wg.cap; i++ {
		chan_wg <- true
	}
	for i := 0; i < n; i++ {
		_ = data[i]
	}
}

// Test that sender synchronizes with receiver even if the sender was blocked.
fn test_no_race_blocked_send_sync() {
	c := chan &Cell[int]{cap: 1}
	c <- &Cell[int](unsafe { nil })
	spawn fn [c] () {
		i := &Cell[int]{
			v: 42
		}
		c <- i
	}()
	// Give the sender time to actually block.
	// This sleep is completely optional: race report must not be printed
	// regardless of whether the sender actually blocks or not.
	// It cannot lead to flakiness.
	time.sleep(10 * time.millisecond)
	_ = <-c
	p := <-c
	if p.v != 42 {
		panic('p.v != 42')
	}
}

// The same as test_no_race_blocked_send_sync above, but sender unblock happens in a select.
fn test_no_race_blocked_select_send_sync() {
	c := chan &Cell[int]{cap: 1}
	c <- &Cell[int](unsafe { nil })
	spawn fn [c] () {
		i := &Cell[int]{
			v: 42
		}
		c <- i
	}()
	time.sleep(10 * time.millisecond)
	_ = <-c
	never := chan int{}
	select {
		p := <-c {
			if p.v != 42 {
				panic('p.v != 42')
			}
		}
		_ := <-never {
		}
	}
}

// Test that close synchronizes with a read from the empty closed channel.
// See https://golang.org/issue/36714.
fn test_no_race_close_happens_before_read() {
	for _ in 0 .. 100 {
		mut loc := &Cell[int]{}
		write := chan Empty{}
		read := chan Empty{}

		spawn fn [loc, write, read] () {
			// Go's `select` with a `default` case receives from a closed channel. V's `select`
			// with an `else` branch skips closed channels, a zero timeout branch does not.
			select {
				_ := <-write {
					_ = loc.v
				}
				0 {
				}
			}
			read.close()
		}()

		spawn fn [mut loc, write] () {
			loc.v = 1
			write.close()
		}()

		_ = <-read
	}
}

// Test that we call the proper race detector function when c.elemsize==0.
// See https://github.com/golang/go/issues/42598
fn test_no_race_elem_size0() {
	mut x := &Cell[int]{}
	mut y := &Cell[int]{}
	c := chan Empty{cap: 2}
	c <- Empty{}
	c <- Empty{}
	spawn fn [mut x, c] () {
		x.v += 1
		_ = <-c
	}()
	spawn fn [mut y, c] () {
		y.v += 1
		_ = <-c
	}()
	time.sleep(10 * time.millisecond)
	c <- Empty{}
	c <- Empty{}
	x.v += 1
	y.v += 1
}
