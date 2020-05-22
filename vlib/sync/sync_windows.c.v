// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module sync

// Mutex HANDLE
type MHANDLE voidptr

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

pub fn (mut m Mutex) lock() {
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

pub fn (mut m Mutex) destroy() {
	if m.state == .waiting {
		m.unlock() // unlock mutex before destroying
	}
	C.CloseHandle(m.mx)  // destroy mutex
	m.state = .destroyed // setting up reference to invalid state
}
