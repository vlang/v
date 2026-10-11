// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module arena

// The arenas live in builtin, in code that only the `builtin_arena` define
// selects. The compiler defines it for every program that imports `arena`.
$if !builtin_arena ? {
	$compile_error('module `arena` needs the `builtin_arena` define, which V sets for programs that import `arena`; pass `-d builtin_arena` to compilers that do not')
}

// Config holds the options for `new`.
@[params]
pub struct Config {
pub:
	// chunk_size is the size of the first chunk in bytes. Later chunks double
	// in size, up to 4MB (or `chunk_size`, when it is bigger).
	chunk_size int = 64 * 1024
}

// Arena is a scoped, per-thread bump allocator. While it is pushed on a
// thread, every allocation that V code on that thread makes through V's
// allocation functions (strings, arrays, maps, string builders, `&T{}`, ...)
// comes from the arena. Its memory is released in bulk by `reset` or `free`.
// An Arena is a handle: do not use a copy of it after the original was freed.
@[noinit]
pub struct Arena {
mut:
	handle voidptr
}

// new creates an arena. It does not allocate any arena memory before its
// first use. Release it with `free` when it is no longer needed.
pub fn new(config Config) Arena {
	$if prealloc {
		$compile_error('module `arena` does not support `-prealloc`; use `prealloc_scope_begin()` instead')
	}
	$if vgc ? {
		$compile_error('module `arena` supports `-gc none` and the default Boehm GC, not `-gc vgc`')
	}
	return Arena{
		handle: unsafe { v_arena_new(isize(config.chunk_size)) }
	}
}

// push makes `a` the allocator of the current thread, until the matching
// `pop`. Pushes nest: the arena pushed last serves the allocations. An arena
// can be pushed on only one thread at a time. Spawned threads start without
// an active arena.
pub fn (a &Arena) push() {
	unsafe { v_arena_push(a.handle) }
}

// pop restores the allocator that was active before `a` was pushed. It panics,
// when `a` is not the innermost active arena of the current thread. Memory
// allocated in `a` stays valid until `a.reset()` or `a.free()`; use `.clone()`
// after `pop` to copy a value out of the arena.
pub fn (a &Arena) pop() {
	unsafe { v_arena_pop(a.handle) }
}

// reset releases all memory allocated in `a`, so that the arena can be reused.
// It keeps one chunk, so an arena that is reset in a loop stops allocating once
// it has grown to the size the loop needs. It panics, when `a` is still pushed.
pub fn (mut a Arena) reset() {
	unsafe { v_arena_reset(a.handle) }
}

// free releases `a` and all memory allocated in it. It panics, when `a` is
// still pushed. Using `a` after `free` panics, while calling `free` again does
// nothing.
pub fn (mut a Arena) free() {
	if a.handle == unsafe { nil } {
		return
	}
	unsafe { v_arena_free(a.handle) }
	a.handle = unsafe { nil }
}

// used returns the number of bytes allocated in `a` since it was created or
// last reset.
pub fn (a &Arena) used() isize {
	used, _ := v_arena_stats(a.handle)
	return used
}

// capacity returns the number of bytes in the chunks that `a` holds.
pub fn (a &Arena) capacity() isize {
	_, capacity := v_arena_stats(a.handle)
	return capacity
}

// owns reports whether `ptr` points into memory allocated in `a`.
pub fn (a &Arena) owns(ptr voidptr) bool {
	return v_arena_owns(a.handle, ptr)
}

// is_current reports whether `a` is the innermost active arena of the current
// thread, i.e. whether it serves the allocations of the thread right now.
pub fn (a &Arena) is_current() bool {
	return a.handle != unsafe { nil } && v_arena_current() == a.handle
}

// active reports whether the current thread allocates from an arena.
pub fn active() bool {
	return v_arena_current() != unsafe { nil }
}
