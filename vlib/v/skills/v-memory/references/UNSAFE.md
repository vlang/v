# unsafe and C interop

> The rule is in the parent skill. This reference covers: what `unsafe` does not
> check, the C interop conventions, and the ways it goes wrong.

## What unsafe actually means

`unsafe` tells the checker to stop looking. It does not add a runtime check, and it
does not make anything faster. It removes the guarantee that the code is sound.

What the checker will no longer verify inside the block:

- pointer arithmetic and indexing
- that a pointer is non-null and aligned
- that a C string is NUL-terminated
- that a struct layout matches what C expects
- that a pointer still refers to live memory

Anything you assert in a comment, you are asserting instead of proving.

## The discipline

The V repository's own rule, and a good one:

> Keep the block as small as possible, and add a comment explaining why it is
> necessary.

Concretely:

1. **One `unsafe` block per function.** If a function needs two, it is doing two
   jobs; split it so each has one, and each is reviewable on its own.
2. **Never wrap ordinary V logic.** The block is for the pointer work, not for a
   loop that happens to contain a pointer.
3. **Comment the soundness argument**, not the mechanics. "C requires a
   NUL-terminated buffer" is useful; "calloc the buffer" is not.
4. **Keep it in a `.c.v` file** when the interop is platform-specific, so the
  dependency is visible rather than scattered.

```v ignore
// buffer.c.v
module main

// fill zeroes `len` bytes of raw memory. The caller guarantees the buffer is at
// least `len` bytes, which the type cannot express.
fn fill_raw(data voidptr, len usize) {
	unsafe {
		C.memset(data, 0, len)
	}
}
```

## Where interop belongs

The compiler can enforce the separation if you let it. Plain `.v` files should not
contain `C.` or `JS.` calls; the platform-specific ones go in `*_c.v` or `*_js.v`
files, and the compiler then knows which code cannot build where.

`-Wimpure-v` turns an interop call that escaped into a plain `.v` file into a
warning, which is worth having on in CI:

```bash
v -Wimpure-v -check file.v
```

## Passing a V pointer to C

The rules the C side does not enforce for you:

- **It must not outlive the V object.** A pointer kept after the V value goes out
  of scope is a use-after-free.
- **It must not free the memory.** Freeing V memory from C, or letting C free and V
  free again, is a double free.
- **It must not move it.** A `&mut` array can be reallocated by a later append; a
  pointer C is holding then points at the old buffer.
- **A V string is not a C string.** V's `string` carries a length; C wants a NUL.
  Pass `&char` for a NUL-terminated literal, or send the length too.

```v ignore
import unsafe

// c_string returns `s` NUL-terminated. Valid only while the pointer is used
// immediately; the memory belongs to V and must not be freed by C.
pub fn c_string(s string) &char {
	mut copy := s.bytestr()
	unsafe {
		copy = &char(copy.c_str())
	}
	return copy
}
```

## `&char` and the C string types

V has two string representations and confusing them is the most common interop
error:

| V type | Meaning |
| --- | --- |
| `string` | a pointer and a length; may contain no NUL |
| `&char` | a NUL-terminated pointer; no length |

A C API expecting `const char*` wants `&char`. A C API taking `(char*, size_t)`
wants a `[]u8` plus its length.

## unsafe C symbols

C interop declarations go in a `.c.v` file, and use the `const_` prefix for names
that would otherwise collide with V keywords:

```v ignore
// syscall.c.v
module main

fn const_TYPE() int
fn const_NEW() int
```

Using the prefix keeps the declaration legal in V and keeps it from shadowing a
built-in.

## Checking it

```bash
v -check file.c.v            # compiles the interop as C
v -Wimpure-v -check file.v   # catches interop that escaped its .c.v
v -race run main.v           # a race through an unsafe pointer is still a race
```

An `unsafe` block that wraps a lock bug will not be reported by the compiler and
may not be reported by the race detector. That is the cost, and the reason the
blocks are kept short.