# Options and Results in depth

> The decision table is in the parent skill. This reference covers: the unwrapping
> forms, propagating an error, and when a default value is honest.

`?T` is an **option**: the value may be absent, and absence is a normal answer.
`!T` is a **result**: the operation can fail, and the failure carries a reason.

## Choosing one

| Question | Answer | Type |
| --- | --- | --- |
| Can this legitimately have no value? | absence is a normal answer | `?T` |
| Does this touch the filesystem, network or a caller? | it can fail | `!T` |
| Does the caller need to know *why* it failed? | `!T` | `!T` |
| Is absence a bug rather than an outcome? | use `!T` and say so | `!T` |
| Is this a lookup in a map or a struct field? | `?T` | `?T` |

Prefer `!T` as the default. An option forces every caller to invent a meaning for
`none`; a result at least makes them read the message.

## The unwrapping forms

```v
import os

fn config_size(path string) !int {
	// 1. Propagate as is: `!` hands the error to the caller unchanged. It needs the
	//    enclosing function to return a result.
	text := os.read_file(path)!
	return text.len
}

fn checked_size(path string) !int {
	// 2. Propagate with context. `err` is in scope inside the block of a result.
	return config_size(path) or { return error('config ${path}: ${err.msg()}') }
}

fn main() {
	// 3. Handle and leave: the block exits the enclosing function.
	size := checked_size('app.conf') or {
		eprintln('skipping: ${err.msg()}')
		return
	}
	// 4. Default: the block supplies a replacement value. This works on a result
	//    as well as on an option.
	limit := checked_size('limits.conf') or { 100 }
	// 5. Abort, only where a value is needed unconditionally, and never in a
	//    library that a caller depends on.
	home := os.getenv_opt('HOME') or { panic('HOME is not set') }
	println('${size} ${limit} ${home}')
}
```

The block of `or` either produces a value of the unwrapped type or leaves: with
`return`, `break`, `continue` or `panic`. A `return` there follows the enclosing
function's return type, not the type of the call that failed. `err` exists only
in the block of a result: an option has no error to name, so its block cannot use
`err`.

## Propagating without adding noise

`!` (or `?` for an option inside an option-returning function) is the short form,
and correct when the error already says everything. When it does not, add what the
caller cannot see:

```v ignore
// Not useful: the message already names the file.
f(path)!

// Useful: the caller does not know which of three files failed.
f(path) or { return error('config: ${err.msg()}') }
```

## `?T` returning `!T`

`?` only propagates out of a function that returns an option, and `!` only out of
one that returns a result: neither shape converts implicitly. `main` and test
functions are the exception: there, `x!` and `x?` unwrap or end the program with
the error. Anywhere else, make the boundary explicit:

```v ignore
// A lookup that is absent is an error here, because this caller requires a value.
port := find_port(cfg) or { return error('no port configured') }
```

## Smart casts

Inside `if x != none`, V narrows `x`, so the unwrapped value is available without
a second unwrap:

```v
struct Config {
	port ?int
}

fn main() {
	cfg := Config{
		port: 8080
	}
	if cfg.port != none {
		println(cfg.port + 1) // int, not ?int
	}
	// An `if` guard does the same and skips the body when there is no value:
	if port := cfg.port {
		println(port)
	}
}
```

The guard is the idiomatic form, because it cannot be forgotten.

## Rules that are not negotiable

- In a function returning `!` (no value), a bare `return` means success; failure
  is `return error(...)`.
- Do not return `0`, `''` or `-1` to mean failure. A caller cannot tell that apart
  from a real value, and neither can a test.
- Do not swallow an error with an empty `or {}` unless a comment says why it is
  safe.

## Testing them

Assert on presence or absence, not on the message text, which is free to change:

```v ignore
assert find_port(cfg) == none
// or, when you need the value:
if port := find_port(cfg) {
	assert port == 8080
}
```

See [v-testing](../../v-testing/SKILL.md) for the rest of the assertion forms.
