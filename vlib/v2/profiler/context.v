// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module profiler

import context

// Allocator interface - can be swapped at runtime
// Similar to Jai's context.allocator design
pub struct Allocator {
pub:
	alloc_fn   fn (size int, ctx voidptr) voidptr                  = unsafe { nil }
	free_fn    fn (ptr voidptr, ctx voidptr)                       = unsafe { nil }
	realloc_fn fn (ptr voidptr, new_size int, ctx voidptr) voidptr = unsafe { nil }
	ctx        voidptr // User data for allocator implementation
}

// Thread-local context (implicit parameter).
// This allows all allocations in a scope to be tracked without explicit annotations.
// A `@[thread_local]` global can only be const-initialized — a runtime initializer
// (like `Context{}`, whose default allocator is built in `_vinit()`) would run once on
// the main thread, so other threads would see a zeroed value. Each thread therefore
// starts with a zeroed context, and `ctx()` lazily installs the default allocator the
// first time it is used on that thread.

@[thread_local]
__global context Context

pub struct Context {
pub mut:
	allocator Allocator
}

// Default allocator - just wraps malloc/free/realloc
pub const default_allocator = Allocator{
	alloc_fn:   default_alloc
	free_fn:    default_free
	realloc_fn: default_realloc
	ctx:        unsafe { nil }
}

fn default_alloc(size int, ctx voidptr) voidptr {
	return unsafe { C.malloc(size) }
}

fn default_free(ptr voidptr, ctx voidptr) {
	unsafe { C.free(ptr) }
}

fn default_realloc(ptr voidptr, new_size int, ctx voidptr) voidptr {
	return unsafe { C.realloc(ptr, new_size) }
}

fn normalize_allocator(a Allocator) Allocator {
	return Allocator{
		alloc_fn:   if isnil(a.alloc_fn) { default_alloc } else { a.alloc_fn }
		free_fn:    if isnil(a.free_fn) { default_free } else { a.free_fn }
		realloc_fn: if isnil(a.realloc_fn) { default_realloc } else { a.realloc_fn }
		ctx:        a.ctx
	}
}

// ctx returns this thread's context, installing the default allocator on first use.
// Thread-local globals are zero-initialized per thread, so a fresh thread's context
// has nil allocator function pointers until this runs.
@[inline]
fn ctx() &Context {
	if isnil(context.allocator.alloc_fn) || isnil(context.allocator.free_fn)
		|| isnil(context.allocator.realloc_fn) {
		context.allocator = normalize_allocator(context.allocator)
	}
	return &context
}

// User code calls these - they use context.allocator implicitly
// This provides the Jai-like implicit allocation tracking
pub fn alloc(size int) voidptr {
	c := ctx()
	return c.allocator.alloc_fn(size, c.allocator.ctx)
}

pub fn free(ptr voidptr) {
	c := ctx()
	c.allocator.free_fn(ptr, c.allocator.ctx)
}

pub fn realloc(ptr voidptr, new_size int) voidptr {
	c := ctx()
	return c.allocator.realloc_fn(ptr, new_size, c.allocator.ctx)
}

// Scoped allocator helper - saves and restores the allocator
// Usage:
//   old := profiler.push_allocator(my_allocator)
//   defer { profiler.pop_allocator(old) }
//   // ... all allocations here use my_allocator
pub fn push_allocator(a Allocator) Allocator {
	mut c := ctx()
	old := c.allocator
	c.allocator = normalize_allocator(a)
	return old
}

pub fn pop_allocator(old Allocator) {
	context.allocator = old
}
