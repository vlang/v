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
* Values stored into longer lived places while an arena is active (globals, caches, outer
  arrays that grow inside the scope, errors returned from the scope) point into the arena,
  and dangle after `reset()` or `free()`. Initialize lazily created global state before
  pushing an arena.
* `reset()` and `free()` panic when the arena is still pushed. Using an arena after `free()`
  panics. Do not use a copy of an `Arena` after the original was freed.
* Pop every arena before its thread ends.

## Supported builds

* `-gc none`: the main use case.
* The default Boehm GC: arena chunks are allocated as uncollectable, but scanned memory, so
  GC objects that are only referenced from arena memory stay alive.
* `-prealloc` and `-gc vgc` are not supported, and using `arena` with them is a compile
  time error. `-prealloc` has its own scopes (`prealloc_scope_begin()`).

Programs that never create an arena only pay one global check per allocation and `free()`.
