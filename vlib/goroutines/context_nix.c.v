// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Platform-specific context switching using ucontext (Linux, macOS, BSDs).
// This provides the low-level mechanism for goroutine context switches,
// analogous to Go's gogo/gosave assembly routines.
module goroutines

#flag -D_XOPEN_SOURCE=700
#flag darwin -D_DARWIN_C_SOURCE
#include "@VMODROOT/vlib/goroutines/context_nix.h"

fn C.goroutines_context_alloc() voidptr
fn C.goroutines_context_init(ctx voidptr, stack voidptr, stack_size int, entry_fn voidptr, arg voidptr)
fn C.goroutines_context_switch(from voidptr, to voidptr)
fn C.goroutines_context_set(to voidptr)

// Context wraps ucontext_t for goroutine context switching.
// The actual ucontext_t is heap-allocated in C to avoid issues with
// ucontext_t being a typedef rather than a struct tag.
pub struct Context {
pub mut:
	uctx voidptr // heap-allocated ucontext_t
}

// context_init initializes a context for a new goroutine.
// Sets up the context to run `entry_fn` with `arg` on the given stack.
pub fn context_init(mut ctx Context, stack voidptr, stack_size int, entry_fn fn (voidptr), arg voidptr) {
	if ctx.uctx == unsafe { nil } {
		ctx.uctx = C.goroutines_context_alloc()
	}
	C.goroutines_context_init(ctx.uctx, stack, stack_size, voidptr(entry_fn), arg)
}

// context_switch saves the current context into `from` and switches to `to`.
pub fn context_switch(mut from Context, to &Context) {
	if from.uctx == unsafe { nil } {
		from.uctx = C.goroutines_context_alloc()
	}
	C.goroutines_context_switch(from.uctx, to.uctx)
}

// context_set switches to the given context without saving.
pub fn context_set(to &Context) {
	C.goroutines_context_set(to.uctx)
}
