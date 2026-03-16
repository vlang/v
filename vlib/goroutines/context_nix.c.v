// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
// Platform-specific context switching using ucontext (Linux, macOS, BSDs).
// This provides the low-level mechanism for goroutine context switches,
// analogous to Go's gogo/gosave assembly routines.
module goroutines

#include <ucontext.h>

// ucontext_t - POSIX context structure for cooperative context switching
struct C.ucontext_t {
mut:
	uc_link    &C.ucontext_t
	uc_stack   C.stack_t
}

struct C.stack_t {
mut:
	ss_sp    voidptr
	ss_size  usize
	ss_flags int
}

fn C.getcontext(ucp &C.ucontext_t) int
fn C.setcontext(ucp &C.ucontext_t) int
fn C.makecontext(ucp &C.ucontext_t, func fn (), argc int, ...voidptr)
fn C.swapcontext(oucp &C.ucontext_t, ucp &C.ucontext_t) int

// Context wraps ucontext_t for goroutine context switching.
pub struct Context {
pub mut:
	uctx C.ucontext_t
}

// context_init initializes a context for a new goroutine.
// Sets up the context to run `entry_fn` with `arg` on the given stack.
pub fn context_init(mut ctx Context, stack voidptr, stack_size int, entry_fn fn (voidptr), arg voidptr) {
	C.getcontext(&ctx.uctx)
	ctx.uctx.uc_stack.ss_sp = stack
	ctx.uctx.uc_stack.ss_size = usize(stack_size)
	ctx.uctx.uc_link = unsafe { nil }
	// makecontext with the goroutine trampoline
	// We pass the arg as two 32-bit ints to be portable (makecontext uses int args)
	lo := u32(u64(arg))
	hi := u32(u64(arg) >> 32)
	C.makecontext(&ctx.uctx, fn [entry_fn, lo, hi] () {
		combined := voidptr(u64(lo) | (u64(hi) << 32))
		entry_fn(combined)
	}, 0)
}

// context_switch saves the current context into `from` and switches to `to`.
pub fn context_switch(mut from Context, to &Context) {
	C.swapcontext(&from.uctx, &to.uctx)
}

// context_set switches to the given context without saving.
pub fn context_set(to &Context) {
	C.setcontext(&to.uctx)
}
