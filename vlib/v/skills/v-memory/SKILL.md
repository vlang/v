---
name: v-memory
description: V's memory model - GC modes (-gc boehm, -gc none, -prealloc), the optional -ownership checking that catches use-after-move, what unsafe unlocks, and the C interop rules. Use when choosing a GC mode, when a program leaks or grows without bound, when deciding whether unsafe is worth it, when passing a V pointer to C, or when a move or borrow error appears. Does not cover general language rules (see v-lang), data structure choice (see v-lang's structs and maps section), profiling (see v-workflow), scripting a task in V (see v-scripts), or the wider command surface (see v-tools).
license: MIT
---

# Memory in V

The default needs no thought: the Boehm collector traces and frees what is no
longer reachable. Every question below only arises when you leave that default.

## Resource Routing

- `references/OWNERSHIP.md` - Read when `-ownership` is in play, or when you want
  a use-after-move caught at compile time rather than at 3am.
- `references/UNSAFE.md` - Read before writing any `unsafe` block, and before
  passing a V pointer into C.

## Quick Reference

| Situation | Mode |
| --- | --- |
| An ordinary program | default (Boehm GC) — do nothing |
| A short-lived batch job, or a self-contained binary | `-gc none` |
| A compiler or batch tool | `-prealloc` |
| You want use-after-move caught at compile time | `-ownership` |
| Talking to a C library | `unsafe`, and keep it in one place |
| Tuning, before guessing | `-stats run` |

**Default**: leave the GC alone. `-gc none` and `-prealloc` are not performance
switches; they remove the collector and move the responsibility to you.

## The three modes

```bash
v run main.v                    # Boehm tracing GC
v -gc none -prod main.v         # no collector at all
v -prealloc -prod main.v        # arena allocation
```

| Mode | Frees | Cost |
| --- | --- | --- |
| Boehm (default) | unreachable memory | tracing pauses; memory is not returned promptly |
| `-gc none` | nothing, unless you do | you must free; no cycles; no finalisers |
| `-prealloc` | everything, at exit | only good for one-shot single-threaded work |

`-gc none` means no cycle collection, so a reference cycle leaks. For a program
that runs once and exits — a compiler, a code generator, a batch script — that
trade is usually worth it.

`-prealloc` is documented as suitable only for short-lived, single-threaded,
batch-like programs. Do not reach for it in a server.

**Race builds are the exception to all of it.** `-race` does not work with `-gc
none` or `-prealloc`: ThreadSanitizer has to see every allocation and free, which
a collector or an arena hides. So a program you want to race-test is a program you
must run under the default GC. It also needs `clang` or `gcc` with the
ThreadSanitizer runtime, and prefers `clang`, because gcc does not instrument
copies of whole struct values such as strings and arrays, so it misses races on
them.

## Check before you optimise

```bash
v -stats run main.v
```

That reports allocations. `-gc none` on a program that allocates heavily and
exits quickly can be a large win; on a long-running one it is a leak generator.
Measure first.

## Ownership: catching a move at compile time

V has an optional ownership system, off by default, enabled with `-ownership`. It
tracks owned values and rejects using one after it has been moved:

```v ignore
fn main() {
	s1 := 'hello'.to_owned()
	s2 := s1          // s1 is moved into s2
	println(s1)       // error: use of moved value: `s1`
}
```

Only strings created with `.to_owned()` participate; literals and primitives are
unaffected. So it is a targeted tool for the case it covers, not a general
guarantee. `doc/ownership.md` is the reference; `references/OWNERSHIP.md` here is
the summary.

```bash
v -ownership file.v        # check and compile
```

It also defines a `ownership` compile-time flag, so `$if ownership ? { ... }` and
`*_d_ownership.v` files select the enabled behaviour.

## unsafe

`unsafe` exists for C interop and for a handful of low-level primitives. It is not
a way to make the compiler stop complaining.

The rule: one `unsafe` block, as small as possible, with a comment saying why it is
sound. If a function needs two, split it so each has one.

```v ignore
import unsafe

// Fixed-size stack buffer; the size must be a compile-time constant.
pub fn (b &Buf) fill(c byte) {
	unsafe {
		b.data = char(c)
		b.len = b.size
	}
}
```

See `references/UNSAFE.md` for what it does not check and what that costs.

## C interop

V's own rule is the one to follow: avoid `C.` and `JS.` in plain `.v` files and put
the interop in `.c.v` or `.js.v` files. Then the compiler can tell which code is
platform-specific, and `-Wimpure-v` catches an interop call that escaped.

```v ignore
// buffer_windows.c.v
module main

fn windows_only() int {
	unsafe {
		return C.GetLastError()
	}
}
```

A V pointer handed to C must not be freed or moved by the caller, and the C side
must not retain it after the V object goes out of scope. See `references/UNSAFE.md`.

## Validation

```bash
v -ownership file.v            # catch a move at compile time
v -stats run main.v            # measure before changing the mode
v -check main.v                # plain, portable V
```

For library code add `-shared`. See [v-workflow](../v-workflow/SKILL.md).

## Related Skills

- **The language rules**: see [v-lang](../v-lang/SKILL.md) for `mut` and for when
  to reach for a struct rather than a map, which changes how much is allocated.
- **The build loop**: see [v-workflow](../v-workflow/SKILL.md) for `-prod`,
  `-stats` and the other flags this skill uses.
- **Concurrency**: see [v-concurrency](../v-concurrency/SKILL.md) for the shared
  state whose lifetime a lock has to protect.