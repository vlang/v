// vtest build: windows
// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Windows fiber backend tests for the goroutines module.
// These tests are deliberately sequential and driven from the main thread,
// so a single worker M serves them deterministically.
module goroutines

import time

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
