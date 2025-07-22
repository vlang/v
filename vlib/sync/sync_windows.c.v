// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module sync

import time

#include <synchapi.h>
#include <time.h>

fn C.GetSystemTimeAsFileTime(lpSystemTimeAsFileTime &C._FILETIME)
fn C.InitializeConditionVariable(voidptr)
fn C.WakeConditionVariable(voidptr)
fn C.SleepConditionVariableSRW(voidptr, voidptr, u32, u32) int

// TODO: The suggestion of using CriticalSection instead of mutex
// was discussed. Needs consideration.

// Mutex HANDLE
type MHANDLE = voidptr

// Semaphore HANDLE
type SHANDLE = voidptr

@[typedef]
pub struct C.CONDITION_VARIABLE {}

//[init_with=new_mutex] // TODO: implement support for this struct attribute, and disallow Mutex{} from outside the sync.new_mutex() function.

// `SRWLOCK` is much more performant that `Mutex` on Windows, so use that in both cases since we don't
// want to share with other processes
@[heap]
pub struct Mutex {
mut:
	mx C.SRWLOCK // mutex handle
}

@[heap]
pub struct RwMutex {
mut:
	mx C.SRWLOCK // mutex handle
}

@[heap]
pub struct Semaphore {
	mtx  C.SRWLOCK
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

pub fn (mut m Mutex) lock() {
	C.AcquireSRWLockExclusive(&m.mx)
}

// try_lock try to lock the mutex instance and return immediately.
// If the mutex was already locked, it will return false.
// NOTE: try_lock require Windows 7 or later. Before Windows 7, it will always return false.
// NOTE: To enable try_lock , you should compile your project with `-d windows_7`, like `v . -d windows_7`
// https://learn.microsoft.com/en-us/windows/win32/api/synchapi/nf-synchapi-tryacquiresrwlockexclusive
pub fn (mut m Mutex) try_lock() bool {
	$if windows_7 ? {
		return C.TryAcquireSRWLockExclusive(&m.mx) != 0
	} $else {
		return false
	}
}

pub fn (mut m Mutex) unlock() {
	C.ReleaseSRWLockExclusive(&m.mx)
}

// RwMutex has separate read- and write locks
pub fn (mut m RwMutex) rlock() {
	C.AcquireSRWLockShared(&m.mx)
}

pub fn (mut m RwMutex) lock() {
	C.AcquireSRWLockExclusive(&m.mx)
}

// try_rlock try to lock the given RwMutex instance for reading and return immediately.
// If the mutex was already locked, it will return false.
// NOTE: try_rlock require Windows 7 or later. Before Windows 7, it will always return false.
// NOTE: To enable try_rlock , you should compile your project with `-d windows_7`, like `v . -d windows_7`
// https://learn.microsoft.com/en-us/windows/win32/api/synchapi/nf-synchapi-tryacquiresrwlockshared
pub fn (mut m RwMutex) try_rlock() bool {
	$if windows_7 ? {
		return C.TryAcquireSRWLockShared(&m.mx) != 0
	} $else {
		return false
	}
}

// try_wlock try to lock the given RwMutex instance for writing and return immediately.
// If the mutex was already locked, it will return false.
// NOTE: try_wlock require Windows 7 or later. Before Windows 7, it will always return false.
// NOTE: To enable try_wlock , you should compile your project with `-d windows_7`, like `v . -d windows_7`
// https://learn.microsoft.com/en-us/windows/win32/api/synchapi/nf-synchapi-tryacquiresrwlockexclusive
pub fn (mut m RwMutex) try_wlock() bool {
	$if windows_7 ? {
		return C.TryAcquireSRWLockExclusive(&m.mx) != 0
	} $else {
		return false
	}
}

// Windows SRWLocks have different function to unlock
// So provide two functions here, too, to have a common interface
pub fn (mut m RwMutex) runlock() {
	C.ReleaseSRWLockShared(&m.mx)
}

pub fn (mut m RwMutex) unlock() {
	C.ReleaseSRWLockExclusive(&m.mx)
}

@[inline]
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
		if C.atomic_compare_exchange_weak_u32(&sem.count, &c, c + 1) {
			return
		}
	}
	C.AcquireSRWLockExclusive(&sem.mtx)
	c = C.atomic_fetch_add_u32(voidptr(&sem.count), 1)
	if c == 0 {
		C.WakeConditionVariable(&sem.cond)
	}
	C.ReleaseSRWLockExclusive(&sem.mtx)
}

pub fn (mut sem Semaphore) wait() {
	mut c := C.atomic_load_u32(&sem.count)
	for c > 0 {
		if C.atomic_compare_exchange_weak_u32(&sem.count, &c, c - 1) {
			return
		}
	}
	C.AcquireSRWLockExclusive(&sem.mtx)
	c = C.atomic_load_u32(&sem.count)

	outer: for {
		if c == 0 {
			C.SleepConditionVariableSRW(&sem.cond, &sem.mtx, C.INFINITE, 0)
			c = C.atomic_load_u32(&sem.count)
		}
		for c > 0 {
			if C.atomic_compare_exchange_weak_u32(&sem.count, &c, c - 1) {
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
		if C.atomic_compare_exchange_weak_u32(&sem.count, &c, c - 1) {
			return true
		}
	}
	return false
}

pub fn (mut sem Semaphore) timed_wait(timeout time.Duration) bool {
	mut c := C.atomic_load_u32(&sem.count)
	for c > 0 {
		if C.atomic_compare_exchange_weak_u32(&sem.count, &c, c - 1) {
			return true
		}
	}
	mut ft_start := C._FILETIME{}
	C.GetSystemTimeAsFileTime(&ft_start)
	time_end := ((u64(ft_start.dwHighDateTime) << 32) | ft_start.dwLowDateTime) +
		u64(timeout / (100 * time.nanosecond))
	mut t_ms := u32(timeout.sys_milliseconds())
	C.AcquireSRWLockExclusive(&sem.mtx)
	mut res := 0
	c = C.atomic_load_u32(&sem.count)

	outer: for {
		if c == 0 {
			res = C.SleepConditionVariableSRW(&sem.cond, &sem.mtx, t_ms, 0)
			if res == 0 {
				break outer
			}
			c = C.atomic_load_u32(&sem.count)
		}
		for c > 0 {
			if C.atomic_compare_exchange_weak_u32(&sem.count, &c, c - 1) {
				if c > 1 {
					C.WakeConditionVariable(&sem.cond)
				}
				break outer
			}
		}
		C.GetSystemTimeAsFileTime(&ft_start)
		time_now := ((u64(ft_start.dwHighDateTime) << 32) | ft_start.dwLowDateTime) // in 100ns
		if time_now > time_end {
			break outer // timeout exceeded
		}
		t_ms = u32((time_end - time_now) / 10000)
	}
	C.ReleaseSRWLockExclusive(&sem.mtx)
	return res != 0
}

pub fn (mut m RwMutex) destroy() {
	// nothing to do
}

pub fn (mut m Mutex) destroy() {
	// nothing to do
}

pub fn (s Semaphore) destroy() {
	// nothing to do
}
