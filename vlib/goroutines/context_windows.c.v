// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Platform-specific context switching using Windows Fibers.
module goroutines

#include <windows.h>

fn C.CreateFiber(dwStackSize usize, lpStartAddress voidptr, lpParameter voidptr) voidptr
fn C.ConvertThreadToFiber(lpParameter voidptr) voidptr
fn C.SwitchToFiber(lpFiber voidptr)
fn C.DeleteFiber(lpFiber voidptr)
fn C.ConvertFiberToThread()

pub struct Context {
pub mut:
	fiber           voidptr
	is_thread_fiber bool // true if this was created via ConvertThreadToFiber
}

pub fn context_init(mut ctx Context, stack voidptr, stack_size int, entry_fn fn (voidptr), arg voidptr) {
	// Windows fibers manage their own stack, so we ignore the stack param
	ctx.fiber = C.CreateFiber(usize(stack_size), voidptr(entry_fn), arg)
}

pub fn context_switch(mut from Context, to &Context) {
	C.SwitchToFiber(to.fiber)
}

pub fn context_set(to &Context) {
	C.SwitchToFiber(to.fiber)
}

// convert_thread_to_fiber must be called once per OS thread before using fibers.
pub fn convert_thread_to_fiber() Context {
	mut ctx := Context{}
	ctx.fiber = C.ConvertThreadToFiber(unsafe { nil })
	ctx.is_thread_fiber = true
	return ctx
}
