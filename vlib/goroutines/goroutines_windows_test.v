// vtest build: windows
// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Windows fiber backend tests for the goroutines module.
// These tests are deliberately sequential and driven from the main thread,
// so a single worker M serves them deterministically.
module goroutines

import time
import sync.stdatomic

// Fiber stacks are not scanned by Boehm yet; match the existing goroutines suite.
fn testsuite_begin() {
	gc_disable()
}

fn testsuite_end() {
	gc_enable()
}

@[unsafe]
fn worker_set_flag(arg voidptr) {
	if arg == unsafe { nil } {
		return
	}
	unsafe {
		mut done := &bool(arg)
		*done = true
	}
}

fn test_windows_goroutine_runs() {
	mut done := false
	goroutine_create(voidptr(worker_set_flag), voidptr(&done), 0)
	for _ in 0 .. 1000 {
		if done {
			break
		}
		time.sleep(1 * time.millisecond)
	}
	assert done
	time.sleep(100 * time.millisecond)
}

fn worker_send_seven(arg voidptr) {
	c := unsafe { &Chan(arg) }
	mut val := 7
	if !chan_send(c, voidptr(&val), true) {
		panic('worker send failed')
	}
}

fn test_windows_main_recvs_from_worker() {
	c := chan_make(int(sizeof(int)), 0)
	goroutine_create(voidptr(worker_send_seven), voidptr(c), 0)
	mut recv_val := 0
	received, ok := chan_recv(c, voidptr(&recv_val), true)
	if !received || !ok {
		panic('main recv failed')
	}
	if recv_val != 7 {
		panic('main received wrong value')
	}
	time.sleep(100 * time.millisecond)
}

fn worker_recv_eight(arg voidptr) {
	c := unsafe { &Chan(arg) }
	mut recv_val := 0
	received, ok := chan_recv(c, voidptr(&recv_val), true)
	if !received || !ok {
		panic('worker recv failed')
	}
	if recv_val != 8 {
		panic('worker received wrong value')
	}
}

fn test_windows_main_sends_to_worker() {
	c := chan_make(int(sizeof(int)), 0)
	goroutine_create(voidptr(worker_recv_eight), voidptr(c), 0)
	time.sleep(20 * time.millisecond)
	mut val := 8
	if !chan_send(c, voidptr(&val), true) {
		panic('main send failed')
	}
	time.sleep(100 * time.millisecond)
}

fn buffered_worker(arg voidptr) {
	c := unsafe { &Chan(arg) }
	mut total := 0
	for _ in 0 .. 4 {
		mut val := 0
		received, _ := chan_recv(c, voidptr(&val), true)
		if !received {
			panic('buffered recv failed')
		}
		total += val
	}
	if total != 10 {
		panic('buffered total wrong')
	}
}

fn test_windows_buffered_channel() {
	c := chan_make(int(sizeof(int)), 4)
	goroutine_create(voidptr(buffered_worker), voidptr(c), 0)
	for i in 1 .. 5 {
		mut val := i
		if !chan_send(c, voidptr(&val), true) {
			panic('buffered send failed')
		}
	}
	time.sleep(200 * time.millisecond)
}

@[unsafe]
fn test_windows_fiber_lifecycle() {
	mut ctx := Context{}
	assert ctx.uctx == unsafe { nil }
	context_init(mut ctx, unsafe { nil }, 65536, worker_set_flag, unsafe { nil })
	assert ctx.uctx != unsafe { nil }
	context_destroy(mut ctx)
	assert ctx.uctx == unsafe { nil }
	context_init(mut ctx, unsafe { nil }, 65536, worker_set_flag, unsafe { nil })
	assert ctx.uctx != unsafe { nil }
	context_destroy(mut ctx)
	assert ctx.uctx == unsafe { nil }
}

// poll_done waits up to tries * 10 milliseconds for a worker to publish completion.
// Sleeps are coarse: Windows quantizes short sleeps to ~15ms, so 1ms polling
// would stretch a 5s timeout past a minute. Bounded polling keeps a scheduler
// regression a failing test instead of a CI hang.
fn poll_done(flag &u64, tries int) bool {
	for _ in 0 .. tries {
		if stdatomic.load_u64(flag) != 0 {
			return true
		}
		time.sleep(10 * time.millisecond)
	}
	return stdatomic.load_u64(flag) != 0
}

fn test_windows_gqueue() {
	mut q := GoroutineQueue{}
	assert q.empty()
	assert q.size == 0
	mut g1 := &Goroutine{
		id: 1
	}
	mut g2 := &Goroutine{
		id: 2
	}
	mut g3 := &Goroutine{
		id: 3
	}
	q.push_back(g1)
	assert !q.empty()
	assert q.size == 1
	q.push_back(g2)
	q.push_back(g3)
	assert q.size == 3
	gp := q.pop()
	assert gp.id == 1
	assert q.size == 2
	gp2 := q.pop()
	assert gp2.id == 2
	gp3 := q.pop()
	assert gp3.id == 3
	assert q.empty()
}

fn test_windows_glist() {
	mut l := GoroutineList{}
	assert l.empty()
	mut g1 := &Goroutine{
		id: 10
	}
	mut g2 := &Goroutine{
		id: 20
	}
	l.push(g1)
	assert !l.empty()
	assert l.count == 1
	l.push(g2)
	assert l.count == 2
	gp := l.pop()
	assert gp.id == 20
	gp2 := l.pop()
	assert gp2.id == 10
	assert l.empty()
}

fn test_windows_waitq() {
	mut q := WaitQ{}
	assert q.empty()
	mut g1 := &Goroutine{
		id: 1
	}
	mut g2 := &Goroutine{
		id: 2
	}
	mut s1 := &Sudog{
		g: g1
	}
	mut s2 := &Sudog{
		g: g2
	}
	q.enqueue(s1)
	assert !q.empty()
	q.enqueue(s2)
	sg := q.dequeue()
	assert sg.g.id == 1
	sg2 := q.dequeue()
	assert sg2.g.id == 2
	assert q.empty()
}

@[unsafe]
fn test_windows_scheduler_init() {
	gsched.mu.acquire()
	defer {
		gsched.mu.release()
	}
	assert gomaxprocs > 0
	assert gsched.allp.len > 0
	assert gsched.allp.len == int(gomaxprocs)
	// Earlier tests may leave workers running; verify the protected idle list instead.
	mut idle_count := 0
	mut pp := gsched.pidle
	for pp != unsafe { nil } {
		idle_count++
		assert idle_count <= int(gomaxprocs)
		pp = pp.link
	}
	assert idle_count == int(gsched.npidle)
}

@[unsafe]
fn test_windows_nonblocking_empty() {
	c := chan_make(int(sizeof(int)), 0)
	// Unbuffered with no peer: non-blocking ops must fail, not park.
	mut val := 1
	sent := chan_send(c, voidptr(&val), false)
	assert !sent
	mut recv_val := 0
	received, _ := chan_recv(c, voidptr(&recv_val), false)
	assert received == false
	// Nil channel helpers are safe.
	assert chan_len(unsafe { nil }) == 0
	assert chan_cap(unsafe { nil }) == 0
	assert chan_len(c) == 0
	assert chan_cap(c) == 0
}

fn test_windows_buffered_fifo_wrap() {
	c := chan_make(int(sizeof(int)), 3)
	for i in 1 .. 4 {
		mut val := i
		sent := chan_send(c, voidptr(&val), false)
		assert sent
	}
	assert chan_len(c) == 3
	// Full: non-blocking send fails.
	mut extra := 99
	sent := chan_send(c, voidptr(&extra), false)
	assert !sent
	// Drain two, wrapping the read index, then refill past the write index.
	for want in [1, 2] {
		mut recv_val := 0
		received, ok := chan_recv(c, voidptr(&recv_val), false)
		assert received && ok && recv_val == want
	}
	for i in [4, 5] {
		mut val := i
		sent := chan_send(c, voidptr(&val), false)
		assert sent
	}
	for want in [3, 4, 5] {
		mut recv_val := 0
		received, ok := chan_recv(c, voidptr(&recv_val), false)
		assert received && ok && recv_val == want
	}
	assert chan_len(c) == 0
}

fn test_windows_close_drain() {
	c := chan_make(int(sizeof(int)), 2)
	for i in [10, 20] {
		mut val := i
		sent := chan_send(c, voidptr(&val), false)
		assert sent
	}
	chan_close(c)
	// Buffered values survive close with ok.
	for want in [10, 20] {
		mut recv_val := 0
		received, ok := chan_recv(c, voidptr(&recv_val), false)
		assert received && ok && recv_val == want
	}
	// Drained and closed: zero value with ok=false, blocking or not.
	mut recv_val := 0
	received, ok := chan_recv(c, voidptr(&recv_val), false)
	assert received && !ok && recv_val == 0
}

// CloseProbe carries one blocked receiver plus its outcome.
struct CloseProbe {
	c &Chan
mut:
	received bool
	ok       bool
	done     u64
}

fn close_probe_worker(arg voidptr) {
	mut p := unsafe { &CloseProbe(arg) }
	mut v := 0
	received, ok := chan_recv(p.c, voidptr(&v), true)
	p.received = received
	p.ok = ok
	stdatomic.store_u64(&p.done, 1)
}

fn test_windows_close_wakes_receiver() {
	c := chan_make(int(sizeof(int)), 0)
	mut probe := &CloseProbe{
		c: c
	}
	goroutine_create(voidptr(close_probe_worker), voidptr(probe), 0)
	// Let the worker park in recv, then close: it must return with ok=false.
	time.sleep(50 * time.millisecond)
	chan_close(c)
	if !poll_done(&probe.done, 500) {
		panic('receiver did not finish after close')
	}
	assert probe.received
	assert probe.ok == false
}

fn test_windows_main_blocked_close() {
	// Main parks in recv on an unbuffered channel; a worker closes it.
	// The main thread has no scheduler context: it must still wake up.
	c := chan_make(int(sizeof(int)), 0)
	goroutine_create(voidptr(close_chan_worker), voidptr(c), 0)
	mut recv_val := 0
	received, ok := chan_recv(c, voidptr(&recv_val), true)
	assert received
	assert ok == false
	gp := get_current_g()
	assert is_main_g(gp)
}

fn close_chan_worker(arg voidptr) {
	c := unsafe { &Chan(arg) }
	time.sleep(50 * time.millisecond)
	chan_close(c)
}

// FanArg carries one sender's work: send `n` values starting at `base`.
// Main keeps every arg alive in an array: worker fiber stacks are not GC
// roots, so loop-allocated args referenced only by not-yet-started workers
// could otherwise be collected.
struct FanArg {
	c    &Chan
	base int
	n    int
}

fn fan_sender(arg voidptr) {
	a := unsafe { &FanArg(arg) }
	for i in 0 .. a.n {
		mut v := a.base + i
		chan_send(a.c, voidptr(&v), true)
	}
}

fn test_windows_many_sequential() {
	// 100 fire-and-forget sends on one unbuffered channel; blocking recvs
	// pair every send with no sleeps.
	c := chan_make(int(sizeof(int)), 0)
	mut args := []&FanArg{cap: 100}
	for i in 0 .. 100 {
		a := &FanArg{
			c:    c
			base: i
			n:    1
		}
		args << a
		goroutine_create(voidptr(fan_sender), voidptr(a), 0)
	}
	mut total := 0
	for _ in 0 .. 100 {
		mut recv_val := -1
		received, ok := chan_recv(c, voidptr(&recv_val), true)
		assert received && ok
		total += recv_val
	}
	assert total == 4950
}

fn test_windows_fan_in() {
	// Four concurrent senders race on one unbuffered channel; the sum is
	// order-independent, so the test is deterministic without sleeps.
	c := chan_make(int(sizeof(int)), 0)
	mut args := []&FanArg{cap: 4}
	for k in 0 .. 4 {
		a := &FanArg{
			c:    c
			base: k * 100
			n:    25
		}
		args << a
		goroutine_create(voidptr(fan_sender), voidptr(a), 0)
	}
	mut total := 0
	for _ in 0 .. 100 {
		mut recv_val := -1
		received, ok := chan_recv(c, voidptr(&recv_val), true)
		assert received && ok
		total += recv_val
	}
	// Sum over k in 0..4 of (25*(k*100) + sum(0..25)).
	assert total == 4 * 300 + (0 + 1 + 2 + 3) * 2500
}

struct EchoArg {
	req &Chan
	rep &Chan
	n   int
}

fn echo_worker(arg voidptr) {
	a := unsafe { &EchoArg(arg) }
	for _ in 0 .. a.n {
		mut v := 0
		received, ok := chan_recv(a.req, voidptr(&v), true)
		assert received && ok
		mut w := v + 1
		chan_send(a.rep, voidptr(&w), true)
	}
}

fn test_windows_ping_pong() {
	// Repeated two-channel echo over the main thread's blocking handoffs.
	req := chan_make(int(sizeof(int)), 0)
	rep := chan_make(int(sizeof(int)), 0)
	a := &EchoArg{
		req: req
		rep: rep
		n:   20
	}
	goroutine_create(voidptr(echo_worker), voidptr(a), 0)
	for i in 0 .. 20 {
		mut val := i
		sent := chan_send(req, voidptr(&val), true)
		assert sent
		mut recv_val := -1
		received, ok := chan_recv(rep, voidptr(&recv_val), true)
		assert received && ok && recv_val == i + 1
	}
}

fn rendezvous_sender(arg voidptr) {
	c := unsafe { &Chan(arg) }
	mut val := 7
	chan_send(c, voidptr(&val), true)
}

fn test_windows_rendezvous_loop() {
	// Tight worker-send/main-recv loop over fresh channels: every handoff
	// pairs a sender with the blocked main thread, with no sleeps.
	for _ in 0 .. 30 {
		c := chan_make(int(sizeof(int)), 0)
		goroutine_create(voidptr(rendezvous_sender), voidptr(c), 0)
		mut recv_val := 0
		received, ok := chan_recv(c, voidptr(&recv_val), true)
		assert received && ok && recv_val == 7
	}
}

fn sched_yielder(arg voidptr) {
	for _ in 0 .. 200 {
		gosched()
	}
	stdatomic.store_u64(unsafe { &u64(arg) }, 1)
}

fn test_windows_gosched() {
	mut done := u64(0)
	goroutine_create(voidptr(sched_yielder), voidptr(&done), 0)
	if !poll_done(&done, 500) {
		panic('yielding worker did not finish')
	}
}

fn ww_receiver(arg voidptr) {
	p := unsafe { &CloseProbe(arg) }
	mut recv_val := 0
	received, ok := chan_recv(p.c, voidptr(&recv_val), true)
	assert received && ok && recv_val == 7
	stdatomic.store_u64(&p.done, 1)
}

fn ww_sender(arg voidptr) {
	p := unsafe { &CloseProbe(arg) }
	mut val := 7
	chan_send(p.c, voidptr(&val), true)
}

fn test_windows_worker_to_worker() {
	// Receiver parks first on a worker while the main thread only drives
	// creation: exercises worker-to-worker handoff across Ms.
	c := chan_make(int(sizeof(int)), 0)
	mut rprobe := &CloseProbe{
		c: c
	}
	mut sprobe := &CloseProbe{
		c: c
	}
	goroutine_create(voidptr(ww_receiver), voidptr(rprobe), 0)
	time.sleep(20 * time.millisecond)
	goroutine_create(voidptr(ww_sender), voidptr(sprobe), 0)
	if !poll_done(&rprobe.done, 500) {
		panic('worker-to-worker rendezvous did not finish')
	}
	time.sleep(200 * time.millisecond)
}
