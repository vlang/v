// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module sync

import time

// TODO: The suggestion of using CriticalSection instead of mutex
// was discussed. Needs consideration.

// Mutex HANDLE
type MHANDLE = voidptr
// Semaphore HANDLE
type SHANDLE = voidptr

//[init_with=new_mutex] // TODO: implement support for this struct attribute, and disallow Mutex{} from outside the sync.new_mutex() function.

// `SRWLOCK` is much more performant that `Mutex` on Windows, so use that in both cases since we don't want to share with other processes
pub struct Mutex {
mut:
	mx C.SRWLOCK    // mutex handle
}

pub struct RwMutex {
mut:
	mx C.SRWLOCK    // mutex handle
}

pub struct Semaphore {
mut:
	sem SHANDLE
}

pub fn new_mutex() Mutex {
	m := Mutex{}
	C.InitializeSRWLock(&m.mx)
	return m
}

pub fn new_rwmutex() RwMutex {
	m := RwMutex{}
	C.InitializeSRWLock(&m.mx)
	return m
}

pub fn (mut m Mutex) m_lock() {
	C.AcquireSRWLockExclusive(&m.mx)
}

pub fn (mut m Mutex) unlock() {
	C.ReleaseSRWLockExclusive(&m.mx)
}

// RwMutex has separate read- and write locks
pub fn (mut m RwMutex) r_lock() {
	C.AcquireSRWLockShared(&m.mx)
}

pub fn (mut m RwMutex) w_lock() {
	C.AcquireSRWLockExclusive(&m.mx)
}

// Windows SRWLocks have different function to unlock
// So provide two functions here, too, to have a common interface
pub fn (mut m RwMutex) r_unlock() {
	C.ReleaseSRWLockShared(&m.mx)
}

pub fn (mut m RwMutex) w_unlock() {
	C.ReleaseSRWLockExclusive(&m.mx)
}

pub fn (mut m Mutex) destroy() {
	// nothing to do
}

[inline]
pub fn new_semaphore() Semaphore {
	return new_semaphore_init(0)
}

pub fn new_semaphore_init(n u32) Semaphore {
	return Semaphore{
		sem: SHANDLE(C.CreateSemaphore(0, n, C.INT32_MAX, 0))
	}
}

pub fn (s Semaphore) post() {
	C.ReleaseSemaphore(s.sem, 1, 0)
}

pub fn (s Semaphore) wait() {
	C.WaitForSingleObject(s.sem, C.INFINITE)
}

pub fn (s Semaphore) try_wait() bool {
	return C.WaitForSingleObject(s.sem, 0) == 0
}

pub fn (s Semaphore) timed_wait(timeout time.Duration) bool {
	return C.WaitForSingleObject(s.sem, timeout / time.millisecond) == 0
}

pub fn (s Semaphore) destroy() bool {
	return C.CloseHandle(s.sem) != 0
}
