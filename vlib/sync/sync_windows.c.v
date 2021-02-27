// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module sync

import time

#include <synchapi.h>

fn C.InitializeConditionVariable(voidptr)
fn C.WakeConditionVariable(voidptr)
fn C.SleepConditionVariableSRW(voidptr, voidptr, u32, u32) int

// TODO: The suggestion of using CriticalSection instead of mutex
// was discussed. Needs consideration.

// Mutex HANDLE
type MHANDLE = voidptr
// Semaphore HANDLE
type SHANDLE = voidptr

//[init_with=new_mutex] // TODO: implement support for this struct attribute, and disallow Mutex{} from outside the sync.new_mutex() function.

// `SRWLOCK` is much more performant that `Mutex` on Windows, so use that in both cases since we don't want to share with other processes
[heap]
pub struct Mutex {
mut:
	mx C.SRWLOCK    // mutex handle
}

[heap]
pub struct RwMutex {
mut:
	mx C.SRWLOCK    // mutex handle
}

[heap]
struct Semaphore {
	mtx C.SRWLOCK
	cond C.CONDITION_VARIABLE
mut:
	count u32
}

pub fn new_mutex() &Mutex {
	mut m := &Mutex{}
	m.init()
	return m
}

pub fn new_rwmutex() &RwMutex {
	mut m := &RwMutex{}
	m.init()
	return m
}

pub fn (mut m Mutex) init() {
	C.InitializeSRWLock(&m.mx)
}

pub fn (mut m RwMutex) init() {
	C.InitializeSRWLock(&m.mx)
}

pub fn (mut m Mutex) @lock() {
	C.AcquireSRWLockExclusive(&m.mx)
}

pub fn (mut m Mutex) unlock() {
	C.ReleaseSRWLockExclusive(&m.mx)
}

// RwMutex has separate read- and write locks
pub fn (mut m RwMutex) @rlock() {
	C.AcquireSRWLockShared(&m.mx)
}

pub fn (mut m RwMutex) @lock() {
	C.AcquireSRWLockExclusive(&m.mx)
}

// Windows SRWLocks have different function to unlock
// So provide two functions here, too, to have a common interface
pub fn (mut m RwMutex) runlock() {
	C.ReleaseSRWLockShared(&m.mx)
}

pub fn (mut m RwMutex) unlock() {
	C.ReleaseSRWLockExclusive(&m.mx)
}

pub fn (mut m Mutex) destroy() {
	// nothing to do
}

[inline]
pub fn new_semaphore() &Semaphore {
	return new_semaphore_init(0)
}

pub fn new_semaphore_init(n u32) &Semaphore {
	mut sem := &Semaphore{}
	sem.init(n)
	return sem
}

pub fn (mut sem Semaphore) init(n u32) {
	C.atomic_store_u32(&sem.count, n)
	C.InitializeSRWLock(&sem.mtx)
	C.InitializeConditionVariable(&sem.cond)
}

pub fn (mut sem Semaphore) post() {
	mut c := C.atomic_load_u32(&sem.count)
	for c > 1 {
		if C.atomic_compare_exchange_weak_u32(&sem.count, &c, c+1) { return }
	}
	C.AcquireSRWLockExclusive(&sem.mtx)
	c = C.atomic_fetch_add_u32(&sem.count, 1)
	if c == 0 {
		C.WakeConditionVariable(&sem.cond)
	}
	C.ReleaseSRWLockExclusive(&sem.mtx)
}

pub fn (mut sem Semaphore) wait() {
	mut c := C.atomic_load_u32(&sem.count)
	for c > 0 {
		if C.atomic_compare_exchange_weak_u32(&sem.count, &c, c-1) { return }
	}
	C.AcquireSRWLockExclusive(&sem.mtx)
	c = C.atomic_load_u32(&sem.count)
outer:
	for {
		if c == 0 {
			C.SleepConditionVariableSRW(&sem.cond, &sem.mtx, C.INFINITE, 0)
			c = C.atomic_load_u32(&sem.count)
		}
		for c > 0 {
			if C.atomic_compare_exchange_weak_u32(&sem.count, &c, c-1) {
				if c > 1 {
					C.WakeConditionVariable(&sem.cond)
				}
				break outer
			}
		}
	}
	C.ReleaseSRWLockExclusive(&sem.mtx)
}

pub fn (mut sem Semaphore) try_wait() bool {
	mut c := C.atomic_load_u32(&sem.count)
	for c > 0 {
		if C.atomic_compare_exchange_weak_u32(&sem.count, &c, c-1) { return true }
	}
	return false
}

pub fn (mut sem Semaphore) timed_wait(timeout time.Duration) bool {
	mut c := C.atomic_load_u32(&sem.count)
	for c > 0 {
		if C.atomic_compare_exchange_weak_u32(&sem.count, &c, c-1) { return true }
	}
	C.AcquireSRWLockExclusive(&sem.mtx)
	t_ms := u32(timeout / time.millisecond)
	mut res := 0
	c = C.atomic_load_u32(&sem.count)
outer:
	for {
		if c == 0 {
			res = C.SleepConditionVariableSRW(&sem.cond, &sem.mtx, t_ms, 0)
			if res == 0 {
				break outer
			}
			c = C.atomic_load_u32(&sem.count)
		}
		for c > 0 {
			if C.atomic_compare_exchange_weak_u32(&sem.count, &c, c-1) {
				if c > 1 {
					C.WakeConditionVariable(&sem.cond)
				}
				break outer
			}
		}
	}
	C.ReleaseSRWLockExclusive(&sem.mtx)
	return res != 0
}

pub fn (s Semaphore) destroy() bool {
	return true
}
