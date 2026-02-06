// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module profiler

// Allocator interface - can be swapped at runtime
// Similar to Jai's context.allocator design
pub struct Allocator {
pub:
	alloc_fn   fn (size int, ctx voidptr) voidptr                  = default_alloc
	free_fn    fn (ptr voidptr, ctx voidptr)                       = default_free
	realloc_fn fn (ptr voidptr, new_size int, ctx voidptr) voidptr = default_realloc
	ctx        voidptr // User data for allocator implementation
}

// Thread-local context (implicit parameter)
// This allows all allocations in a scope to be tracked without explicit annotations

@[thread_local]
__global context = Context{}

pub struct Context {
pub mut:
	allocator Allocator = default_allocator
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

// User code calls these - they use context.allocator implicitly
// This provides the Jai-like implicit allocation tracking
pub fn alloc(size int) voidptr {
	return context.allocator.alloc_fn(size, context.allocator.ctx)
}

pub fn free(ptr voidptr) {
	context.allocator.free_fn(ptr, context.allocator.ctx)
}

pub fn realloc(ptr voidptr, new_size int) voidptr {
	return context.allocator.realloc_fn(ptr, new_size, context.allocator.ctx)
}

// Scoped allocator helper - saves and restores the allocator
// Usage:
//   old := profiler.push_allocator(my_allocator)
//   defer { profiler.pop_allocator(old) }
//   // ... all allocations here use my_allocator
pub fn push_allocator(a Allocator) Allocator {
	old := context.allocator
	context.allocator = a
	return old
}

pub fn pop_allocator(old Allocator) {
	context.allocator = old
}
