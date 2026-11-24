## Description

`arena` provides scoped, per-thread arena allocators. While an arena is pushed on a thread,
every allocation that V code on that thread makes through V's allocation functions comes
from the arena: strings, arrays, maps, string builders, `&T{}` values, including the ones
made inside other vlib modules. The memory is released in bulk by `reset()` or `free()`.

This gives long-running, multi-threaded programs built with `-gc none` bounded memory use,
without a garbage collector:

```v
import arena
import strings

fn main() {
	mut a := arena.new()
	defer {
		a.free()
	}
	mut longest := ''
	for i in 0 .. 1000 {
		a.push()
		mut sb := strings.new_builder(64)
		for j in 0 .. i % 10 {
			sb.write_string('${i}:${j} ')
		}
		line := sb.str()
		a.pop()
		if line.len > longest.len {
			longest = line.clone() // copy the value out of the arena
		}
		a.reset() // all memory of the iteration is reused in the next one
	}
	println(longest)
}
```

## Rules

* `push()` makes an arena the allocator of the current thread, `pop()` restores the previous
  one. Pushes nest; `pop()` panics, when its arena is not the innermost active arena of the
  thread. Use `defer { a.pop() }` to pop on every return path.
* An arena can be pushed on one thread at a time. Spawned threads start without an active
  arena, so a scope never leaks into another thread by accident.
* Memory allocated in an arena stays valid after `pop()`, until `reset()` or `free()`.
  Copy values that must outlive it with `.clone()` after `pop()`.
* `free()` of arena memory (also by vlib code, like `s.free()` or `arr.free()`) does nothing.
  Growing arena memory (realloc) after `pop()` copies it to the current allocator, so a map
  or array built in an arena can keep growing after the arena was popped.
* Values stored into longer lived places while an arena is active (globals, caches, errors
  returned from the scope) point into the arena, and dangle after `reset()` or `free()`.
  Initialize lazily created global state before pushing an arena.
* Growing an array, map or string builder allocates new storage, so one that was created
  before `push()` and grows inside the scope gets storage in the arena, which dangles after
  `reset()` too. Reserve its capacity before `push()`, or grow it after `pop()`.
* Closures, channels, `shared` values and other objects that are created inside a scope live
  in the arena as well. A thread that is spawned in the scope and still uses them after
  `reset()` or `free()` uses freed memory: wait for such threads before releasing the arena.
* `reset()` and `free()` panic when the arena is still pushed. Using an arena after `free()`
  panics. Do not use a copy of an `Arena` after the original was freed.
* Pop every arena before its thread ends.

## Lifetime tokens

`lifetime_token(ptr)` returns an optional `LifetimeToken` for an address in a live arena chunk.
It returns `none` for non-arena addresses. `token.matches(ptr)` checks whether the address still
belongs to that arena lifetime. Popping an arena preserves its tokens. Resetting or freeing it
invalidates every existing token, including when a retained chunk reuses the same address.

Tokens are copyable values and do not retain an arena or its memory. The queries read allocator
metadata without dereferencing the supplied address or its former arena control block. They can
therefore check a saved address after release, including from another thread.

A token identifies an arena lifetime, rather than an individual allocation: addresses anywhere
in the same live chunk share it. It does not establish that a payload was initialized and does
not pin memory. Synchronize reset/free with payload access; a successful query cannot prevent
another thread from releasing the memory immediately afterward.

## Explicit allocators

`current_allocator()` captures the current thread's allocator, `a.allocator()` selects a
particular arena, and `heap_allocator()` selects V's ordinary allocator. These copyable
`Allocator` handles do not change the current arena. `allocator.is_current()` reports whether
a handle selects the allocator currently serving the thread.

`unsafe { allocator.calloc(size) }` returns zeroed raw storage with V's ordinary allocation
alignment. Zero returns `nil` without allocating; negative sizes panic. An arena may be inactive
or an active ancestor on the calling thread. An arena active on another thread, or already
being modified, rejects allocation. Explicit heap allocation uses V's configured GC or manual
allocator and its existing allocation hooks.

Handles do not retain their arenas. Keep the original arena alive, synchronize reset/free with
allocations and payload access, and never use an arena handle after that arena is freed.
Memory allocated through a handle has the same lifetime and free behavior as ordinary arena
or heap allocations. A handle does not initialize typed V values or keep their pointers alive.

## Supported builds

* `-gc none`: the main use case.
* The default Boehm GC: arena chunks are allocated as uncollectable, but scanned memory, so
  GC objects that are only referenced from arena memory stay alive.
* `-prealloc`: supported. A pushed arena is backed by a `prealloc_scope_*` arena; `reset()`
  rewinds it and `free()` ends it. The lower-level scope API remains available for cross-thread
  handoff and explicit retention, which `arena` does not expose.
* `-gc vgc` is not supported; using `arena` with it is a compile time error.

The allocator hooks are part of builtin in every program. Until a program creates its first
arena, they cost one atomic check per allocation and `free()`.
The first arena publishes all allocator hooks together, including when another thread is allocating.
