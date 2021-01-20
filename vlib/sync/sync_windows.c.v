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

[ref_only]
pub struct Mutex {
mut:
	mx           MHANDLE    // mutex handle
	state        MutexState // mutex state
	cycle_wait   i64        // waiting cycles (implemented only with atomic)
	cycle_woken  i64        // woken cycles    ^
	reader_sem   u32        // reader semarphone
	writer_sem   u32        // writer semaphones
}

[ref_only]
pub struct RwMutex {
mut:
	mx C.SRWLOCK    // mutex handle
}

pub struct Semaphore {
mut:
	sem SHANDLE
}

enum MutexState {
	broken
	waiting
	released
	abandoned
	destroyed
}

pub fn new_mutex() &Mutex {
	sm := &Mutex{}
	unsafe {
		mut m := sm
		m.mx = MHANDLE(C.CreateMutex(0, false, 0))
		if isnil(m.mx) {
			m.state = .broken // handle broken and mutex state are broken
			return sm
		}
	}
	return sm
}

pub fn new_rwmutex() &RwMutex {
	m := &RwMutex{}
	C.InitializeSRWLock(&m.mx)
	return m
}

pub fn (mut m Mutex) m_lock() {
	// if mutex handle not initalized
	if isnil(m.mx) {
		m.mx = MHANDLE(C.CreateMutex(0, false, 0))
		if isnil(m.mx) {
			m.state = .broken // handle broken and mutex state are broken
			return
		}
	}
	state := C.WaitForSingleObject(m.mx, C.INFINITE) // infinite wait
	/* TODO fix match/enum combo
	m.state = match state {
		C.WAIT_ABANDONED { .abandoned }
		C.WAIT_OBJECT_0  { .waiting }
		else           { .broken }
	}
	*/
	if state == C.WAIT_ABANDONED {
		m.state = .abandoned
	// FIXME Use C constant instead
	} else if state == 0 /* C.WAIT_OBJECT_0 */ {
		m.state = .waiting
	} else {
		m.state = .broken
	}
}

pub fn (mut m Mutex) unlock() {
	if m.state == .waiting {
		if C.ReleaseMutex(m.mx) {
			m.state = .broken
			return
		}
	}
	m.state = .released
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
	if m.state == .waiting {
		m.unlock() // unlock mutex before destroying
	}
	C.CloseHandle(m.mx)  // destroy mutex
	m.state = .destroyed // setting up reference to invalid state
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
