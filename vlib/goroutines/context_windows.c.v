// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Platform-specific context switching using Windows Fibers.
module goroutines

#include <windows.h>

fn C.CreateFiber(dwStackSize uint, lpStartAddress voidptr, lpParameter voidptr) voidptr
fn C.ConvertThreadToFiber(lpParameter voidptr) voidptr
fn C.SwitchToFiber(lpFiber voidptr)
fn C.DeleteFiber(lpFiber voidptr)
fn C.ConvertFiberToThread()
fn C.GetCurrentFiber() voidptr

// GetLastError value of ConvertThreadToFiber on a thread that is already a fiber
const error_already_fiber = u32(1280)

pub struct Context {
pub mut:
	uctx            voidptr // fiber handle on Windows
	is_thread_fiber bool    // true if this was created via ConvertThreadToFiber
}

// context_init creates a new fiber that runs `entry_fn` with `arg`.
// A reused dead goroutine has no fiber any more: goexit0 gave it to the scheduler,
// which deleted it (see context_detach_dead).
pub fn context_init(mut ctx Context, stack voidptr, stack_size int, entry_fn fn (voidptr), arg voidptr) {
	// Windows fibers manage their own stack, so we ignore the stack param
	ctx.uctx = C.CreateFiber(uint(stack_size), voidptr(entry_fn), arg)
	if ctx.uctx == unsafe { nil } {
		panic('goroutines: CreateFiber failed, error code ${C.GetLastError()}')
	}
}

pub fn context_switch(mut from Context, to &Context) {
	C.SwitchToFiber(to.uctx)
}

pub fn context_set(to &Context) {
	C.SwitchToFiber(to.uctx)
}

// context_detach_dead takes the fiber out of the context of a goroutine that is exiting.
// That fiber is still running, so it cannot be deleted yet: the scheduler passes the
// returned handle to context_release_dead after it has switched back to g0.
fn context_detach_dead(mut ctx Context) voidptr {
	fiber := ctx.uctx
	ctx.uctx = unsafe { nil }
	return fiber
}

// context_release_dead deletes a fiber returned by context_detach_dead.
fn context_release_dead(fiber voidptr) {
	if fiber != unsafe { nil } {
		C.DeleteFiber(fiber)
	}
}

// current_fiber returns the fiber the calling thread is currently executing on.
pub fn current_fiber() voidptr {
	return C.GetCurrentFiber()
}

// context_destroy deletes an inactive native fiber owned by ctx, if any.
// The currently running fiber and converted thread fibers must not be destroyed.
pub fn context_destroy(mut ctx Context) {
	if ctx.uctx != unsafe { nil } && !ctx.is_thread_fiber {
		C.DeleteFiber(ctx.uctx)
		ctx.uctx = unsafe { nil }
	}
}

// convert_thread_to_fiber must be called once per OS thread before using fibers.
// If other code already made the thread a fiber, it returns that fiber, with
// `is_thread_fiber` false, since the thread is not ours to convert back.
pub fn convert_thread_to_fiber() Context {
	fiber := C.ConvertThreadToFiber(unsafe { nil })
	if fiber != unsafe { nil } {
		return Context{
			uctx:            fiber
			is_thread_fiber: true
		}
	}
	err := C.GetLastError()
	if err != error_already_fiber {
		panic('goroutines: ConvertThreadToFiber failed, error code ${err}')
	}
	// GetCurrentFiber is only valid on a thread that is a fiber, which is the case here.
	return Context{
		uctx: C.GetCurrentFiber()
	}
}

// convert_fiber_to_thread undoes convert_thread_to_fiber when the scheduler of this
// thread stops. It does nothing for a thread that was already a fiber.
fn convert_fiber_to_thread(ctx Context) {
	if ctx.is_thread_fiber {
		C.ConvertFiberToThread()
	}
}
