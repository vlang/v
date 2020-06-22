// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module sync

// We cannot simply use Mutex.lock() in the waitgroup.wait() method
// as it will not block since it's used in the same thread.
// docs:
// Any thread with a handle to a mutex object can use one of the
// wait functions to request ownership of the mutex object.
// If the mutex object is owned by another thread, the wait
// function blocks the requesting thread until the owning thread
// releases the mutex object using the ReleaseMutex function.
// Source:
// https://docs.microsoft.com/en-us/windows/win32/sync/mutex-objects

[ref_only]
struct Waiter{
mut:
	event MHANDLE
}

pub fn (mut w Waiter) wait() {
	C.WaitForSingleObject(w.event, C.INFINITE) // infinite wait
}

pub fn (mut w Waiter) stop() {
	C.SetEvent(w.event)
}
pub fn new_waiter() &Waiter {
	unsafe {
		sm := &Waiter{event: MHANDLE(C.CreateEvent(0, false, true, 0))}
		return sm
	}
}
