// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Atomic operations and C interop for the goroutine scheduler.
module goroutines

#include <stdlib.h>
#include <string.h>
#include "@VMODROOT/vlib/goroutines/goroutines_tls.h"

#flag @VMODROOT/vlib/goroutines/tls.c

// Thread-local storage
fn C.goroutines_get_current_m() voidptr
fn C.goroutines_set_current_m(mp voidptr)

// Typed atomic operations (implemented in tls.c)
fn C.goroutines_atomic_load_u32(ptr &u32) u32
fn C.goroutines_atomic_store_u32(ptr &u32, val u32)
fn C.goroutines_atomic_fetch_add_u32(ptr &u32, val u32) u32
fn C.goroutines_atomic_fetch_add_i32(ptr &i32, val i32) i32
fn C.goroutines_atomic_fetch_sub_i32(ptr &i32, val i32) i32
fn C.goroutines_atomic_fetch_add_u64(ptr &u64, val u64) u64
fn C.goroutines_atomic_cas_u32(ptr &u32, expected &u32, desired u32) bool
fn C.goroutines_atomic_cas_ptr(ptr voidptr, expected voidptr, desired voidptr) bool

fn C.grt_spinlock_lock(lk &i32)
fn C.grt_spinlock_unlock(lk &i32)

fn C.memcpy(dest voidptr, src voidptr, n usize) voidptr
fn C.memset(dest voidptr, ch int, n usize) voidptr
fn C.rand() int

// SpinLock - ucontext-safe lock (pthreads mutex breaks with swapcontext).
pub struct SpinLock {
mut:
	state i32
}

pub fn (mut s SpinLock) acquire() {
	C.grt_spinlock_lock(&s.state)
}

pub fn (mut s SpinLock) release() {
	C.grt_spinlock_unlock(&s.state)
}
