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

Use `arena.try_new()` to handle allocation failure when creating an arena. It returns
`?arena.Arena`. The first chunk is allocated separately on first use.

## Chunk limits

Pass `limits: arena.ChunkLimits{max_live_bytes: ..., max_total_bytes: ...}` when
creating an arena to bound its combined backing chunk allocations. Live bytes
count chunks still held, including the chunk retained by `reset()`. Total bytes
count every successful chunk allocation over the lifetime of the budget.
An omitted limit is unlimited. Zero forbids the first chunk. Growth uses a
smaller chunk when the usual size exceeds the remaining budget and the request
still fits. Implicit allocation panics when a requested chunk cannot fit.

An arena created while a bounded arena is pushed shares its budget, even when
it is later used on another thread. Configuring limits on the child additionally
enforces those limits on its descendants. `chunk_usage()` reports the nearest
budget's live, peak live, and cumulative bytes, including chunk headers. Arena
control blocks, allocator registry metadata, and allocations performed outside
these arenas are outside the counters.

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

## Fallible explicit allocation

`unsafe { allocator.try_calloc(size) }` returns an optional raw pointer. Negative or
unsupported sizes and allocation failure return `none`, without constructing an error.
A zero-size request succeeds with a nil pointer and consumes no storage.
Heap handles use the configured GC or manual heap and return `none` on backing allocation failure,
even while an arena is pushed.

Failed arena growth releases unpublished storage and preserves existing chunks, data,
used bytes and the next regular chunk size. It releases the registry/access locks before
returning; the arena remains usable. A selected arena never falls back to the current
allocator or heap. The lifetime, alignment and thread-access rules of `calloc` still apply;
invalid handles and concurrent access remain programming errors.

```v
import arena

fn main() {
	mut storage := arena.new()
	defer { storage.free() }
	pointer := unsafe { storage.allocator().try_calloc(1024) } or {
		return
	}
	assert storage.owns(pointer)
}
```

Heap allocation uses V's configured manual or Boehm allocator. Failed heap requests do not
create allocation records in opt-in heap tracking. Arena creation and implicit V allocations
retain their existing failure behavior. Callers must check this optional result before using
its storage; this API provides no automatic OOM unwinding or container transaction.

## Running code with an explicit allocator

`unsafe { allocator.run(mut context, work) }` selects the allocator for a synchronous
callback's implicit V allocations and restores the caller's allocator afterward, including
when a panic unwinds the call. The mutable context and a noncapturing function avoid a
mandatory closure allocation. The context itself is not copied into the selected arena.

The target may be the heap, an inactive arena, the current arena, or an ancestor in the
current allocator stack. An arena active outside that stack is rejected. Nested selections
restore in call order. Work must pop any arenas it pushes and balance nested selections.
All involved arenas must remain alive; the handle does not extend any allocation lifetime.
The selected arena's budget also applies to arenas created by the callback.

```v
import arena

struct Message {
mut:
	number int
	text   string
}

fn build_message(mut message Message) {
	message.text = 'selected ${message.number}'
}

fn main() {
	mut storage := arena.new()
	defer { storage.free() }
	mut message := Message{ number: 42 }
	unsafe { storage.allocator().run(mut message, build_message) }
	assert storage.owns(message.text.str)
	assert message.text == 'selected 42'
}
```

## Supported builds

* `-gc none`: the main use case.
* `-freestanding` with the C backend on Linux or macOS amd64/arm64. The selected
  runtime supplies its normal heap and real thread-local arena stacks. Individual
  frees remain no-ops for arena storage; reset/free release backing chunks through
  that runtime's heap. Use a native TLS-capable C compiler such as Clang or GCC.
* The default Boehm GC: arena chunks are allocated as uncollectable, but scanned memory, so
  GC objects that are only referenced from arena memory stay alive.
* `-prealloc` and `-gc vgc` are not supported, and using `arena` with them is a compile
  time error. `-prealloc` has its own scopes (`prealloc_scope_begin()`).
* Other freestanding targets/backends and Vinix do not yet supply the required
  per-thread runtime. Creating a scoped arena there reports that restriction.

The allocator hooks are part of builtin in every program. Until a program creates its first
arena, they cost one atomic check per allocation and `free()`.
The first arena publishes all allocator hooks together, including when another thread is allocating.

## Runtime bookkeeping

The unsafe builtin hooks `v_arena_suspend()` and `v_arena_resume(token)` are for
runtime bookkeeping that must use the configured ordinary allocator. They leave
all pushed arenas active, temporarily select no arena on this thread, and restore
the previous selection. Pair them on the same thread in strict nested order.
Restore the selection before invoking user callbacks; a suspended arena must
remain alive. Prefer the ordinary `arena` API for application allocation.

Closure runtime registries, trampoline-page descriptors and lifetime bookkeeping
use this scope. It does not copy closure captures or extend their lifetimes.
Reclaim or dispose callbacks while every referenced arena is still alive.
The separate scoped `-prealloc` mechanism is unchanged by these hooks; module
`arena` does not support `-prealloc`.
