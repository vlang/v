// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module sync

// Mutex HANDLE
type MHANDLE voidptr

//[init_with=new_mutex] // TODO: implement support for this struct attribute, and disallow Mutex{} from outside the sync.new_mutex() function.
pub struct Mutex {
mut:
	mx           MHANDLE    // mutex handle
	state        MutexState // mutex state
	cycle_wait   i64        // waiting cycles (implemented only with atomic)
	cycle_woken  i64        // woken cycles    ^
	reader_sem   u32        // reader semarphone
	writer_sem   u32        // writer semaphones
}

enum MutexState {
	broken
	waiting
	released
	abandoned
	destroyed
}

const (
	INFINITE = 0xffffffff
)

// Ref - https://docs.microsoft.com/en-us/windows/win32/api/synchapi/nf-synchapi-waitforsingleobject#return-value
const (
	WAIT_ABANDONED     = 0x00000080
	WAIT_IO_COMPLETION = 0x000000C0
	WAIT_OBJECT_0      = 0x00000000
	WAIT_TIMEOUT       = 0x00000102
	WAIT_FAILED        = 0xFFFFFFFF
)

pub fn new_mutex() &Mutex {
	sm := &Mutex{}
	unsafe {
		mut m := sm		
		m.mx = C.CreateMutex(0, false, 0)
		if isnil(m.mx) {
			m.state = .broken // handle broken and mutex state are broken
			return sm
		}		
	}
	return sm
}

pub fn (m mut Mutex) lock() {
	// if mutex handle not initalized
	if isnil(m.mx) {
		m.mx = C.CreateMutex(0, false, 0)
		if isnil(m.mx) {
			m.state = .broken // handle broken and mutex state are broken
			return
		}
	}
	state := C.WaitForSingleObject(m.mx, INFINITE) // infinite wait
	/* TODO fix match/enum combo
	m.state = match state {
		WAIT_ABANDONED { .abandoned }
		WAIT_OBJECT_0  { .waiting }
		else           { .broken }
	}
	*/
	mut s := MutexState.broken
	if state == WAIT_ABANDONED { s = .abandoned }
	else if state == WAIT_OBJECT_0 { s = .waiting }
	m.state = s
}

pub fn (m mut Mutex) unlock() {
	if m.state == .waiting {
		if ReleaseMutex(m.mx) {
			m.state = .broken
			return
		}
	}
	m.state = .released
}

pub fn (m mut Mutex) destroy() {
	if m.state == .waiting {
		m.unlock() // unlock mutex before destroying
	}
	C.CloseHandle(m.mx)  // destroy mutex
	m.state = .destroyed // setting up reference to invalid state
}
