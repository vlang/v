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

pub struct Context {
pub mut:
	uctx            voidptr // fiber handle on Windows
	is_thread_fiber bool    // true if this was created via ConvertThreadToFiber
}

pub fn context_init(mut ctx Context, stack voidptr, stack_size int, entry_fn fn (voidptr), arg voidptr) {
	// Windows fibers manage their own stack, so we ignore the stack param
	ctx.uctx = C.CreateFiber(uint(stack_size), voidptr(entry_fn), arg)
}

pub fn context_switch(mut from Context, to &Context) {
	// Save the current fiber, so this context can be resumed later.
	// This gives fiber switches the same symmetric save/restore semantics as ucontext.
	from.uctx = C.GetCurrentFiber()
	if to.uctx == unsafe { nil } {
		panic('goroutines: SwitchToFiber called with null fiber')
	}
	C.SwitchToFiber(to.uctx)
}

pub fn context_set(to &Context) {
	if to == unsafe { nil } || to.uctx == unsafe { nil } {
		panic('goroutines: SwitchToFiber called with null fiber')
	}
	C.SwitchToFiber(to.uctx)
}

// current_fiber returns the fiber the calling thread is currently executing on.
pub fn current_fiber() voidptr {
	return C.GetCurrentFiber()
}

// context_destroy deletes the native fiber owned by ctx, if any. It must
// only be called when no thread is executing on that fiber, e.g. when a
// dead goroutine is reused for a new one (see newproc1).
pub fn context_destroy(mut ctx Context) {
	if ctx.uctx != unsafe { nil } {
		C.DeleteFiber(ctx.uctx)
		ctx.uctx = unsafe { nil }
	}
}

// convert_thread_to_fiber must be called once per OS thread before using fibers.
pub fn convert_thread_to_fiber() Context {
	mut ctx := Context{}
	ctx.uctx = C.ConvertThreadToFiber(unsafe { nil })
	if ctx.uctx == unsafe { nil } {
		// The thread may already be a fiber (ConvertThreadToFiber fails with
		// ERROR_ALREADY_FIBER then). Reuse the current fiber in that case.
		ctx.uctx = C.GetCurrentFiber()
	}
	if ctx.uctx == unsafe { nil } {
		panic('goroutines: ConvertThreadToFiber failed and thread is not a fiber')
	}
	ctx.is_thread_fiber = true
	return ctx
}
