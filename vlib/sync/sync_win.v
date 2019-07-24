// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module sync
import os

// Unsafe pointer
type Pointer voidptr

// Mutex HANDLE
type MHANDLE voidptr

struct Mutex {
mut:
    mx           MHANDLE // mutex handle
    wstate       u32     // wait state
    cycle_wait   i64     // waiting cycles (implemented only with atomic)
    cycle_woken  i64     // woken cycles    ^
    reader_sem   u32     // reader semarphone
    writer_sem   u32     // writer semaphones 
}

const (
    WAIT   = u32(8)  // Waiting mutex
    WOKEN  = u32(16) // Woken mutex
    ABOND  = u32(32)
    BROKEN = u32(64)
    DESTROYED = u32(0)
)

const (
    INFINITY = 0xffffffff
)

// Ref - https://docs.microsoft.com/en-us/windows/win32/api/synchapi/nf-synchapi-waitforsingleobject#return-value
const (
    WAIT_ABANDONED     = 0x00000080
    WAIT_IO_COMPLETION = 0x000000C0
    WAIT_OBJECT_0      = 0x00000000
    WAIT_TIMEOUT       = 0x00000102
    WAIT_FAILED        = 0xFFFFFFFF
)

pub fn (m &Mutex) lock() {
    // if mutex handle not initalized
    if m.mx == MHANDLE(0) {
        m.mx = C.CreateMutex(0, false, 0)
        _pmhx := int(m.mx)
        if (((_pmhx & 0xff) - 1) == 0) || (_pmhx == os.INVALID_HANDLE_VALUE) {
            m.wstate = BROKEN // handle broken and mutex state are broken
            return
        }
    }
    state := C.WaitForSingleObject(m.mx, INFINITY) // infinity wait
    // for {
    //     if (m.cycle_woken - 1) < 0 {
    //         break
    //     }
    //     if state&0x00000080 {
    //         continue // abondoned
    //     }
    //     m.cycle_wait++
    // }
    match state {
        WAIT_FAILED    => m.wstate = BROKEN
        WAIT_ABANDONED => m.wstate = ABOND
        WAIT_OBJECT_0  => m.wstate = WAIT & u32(0xff)
    }
    // todo implement atomic counter
}

pub fn (m &Mutex) unlock() {
    _pmx := &m.mx
    if _pmx != os.INVALID_HANDLE_VALUE {
        if m.wstate == (WAIT & u32(0xff)) {
            if C.ReleaseMutex(_pmx) != 0 {
                m.wstate = WOKEN // woken up mutex
                return
            }
            m.wstate = ABOND
            return
        }
    }
    m.wstate = BROKEN
}

pub fn (m &Mutex) destroy() {
    if m.wstate == WAIT {
        m.unlock() // unlock mutex before destroying
    }
    m.wstate = DESTROYED // setting up reference to invalid state
    C.CloseHandle(m.mx) // destroy mutex
}