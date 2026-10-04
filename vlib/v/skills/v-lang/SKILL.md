---
name: v-lang
description: The V language rules that make code which looks right fail to compile or behave wrongly - ?T versus !T, sum types and match exhaustiveness, mut receivers, module names matching directories, comptime $ forms, and explicit type conversion. Use when writing or reviewing any V code that uses an Option, a Result, a sum type, a match, generics, or a custom flag, and when a V compiler error names an option, a result, exhaustiveness, mutability, or a module. Does not cover the build and test loop (see v-workflow), writing tests (see v-testing), reading a V project through the MCP server (see v-mcp), scripting in V (see v-scripts), or the wider command surface (see v-tools).
license: MIT
---

# The V rules that bite

Most mistakes an agent makes in V are not algorithmic. They are these rules, and
each one fails in a way that does not look like the mistake.

## Resource Routing

- `references/OPTION-RESULT.md` - Read when deciding whether a function returns
  `?T` or `!T`, or when an `or {}` block is not doing what you expected.
- `references/SUMTYPES.md` - Read when modelling a closed set of states, or when
  `match` reports a non-exhaustive match.
- `references/COMPTIME.md` - Read when a symbol only exists on some platforms, or
  when reaching for reflection or `$embed_file`.
- `references/MUTABILITY.md` - Read when an assignment is rejected, or when a
  mutation to a field does not reach the caller.

## Quick Reference

| You want | Write | Not |
| --- | --- | --- |
| A value that may be absent | `fn f() ?int` | `return 0` as a sentinel |
| An operation that can fail | `fn f() !int` | `return -1` |
| Unwrap, no default | `x or { return error('...') }` | `x or { 0 }` on a result |
| Unwrap, with a default | `x or { 0 }` on an option | `x or { return }` on an option |
| Branch on a closed set | `enum` + exhaustive `match` | a string constant |
| Mutate an argument | `fn f(mut x T)` | `x = ...` silently failing |
| Mutate the caller's struct | `fn (mut t &T)` | `fn (mut t T)` (a copy) |
| Run code at compile time | `$if`, `$for` | a runtime `if` |

## `?T` is not `!T`

`?T` holds a value or `none`. `!T` holds a value or an error. They unwrap
differently, and using one where the other belongs is a type error rather than a
logic error.

```v ignore
// An option: absence is part of the answer.
fn find_port(cfg Config) ?int {
    if !cfg.has_port {
        return none
    }
    return cfg.port
}

// A result: the operation can fail, and the failure carries why.
fn read_port(path string) !int {
    lines := os.read_lines(path) or {
        return error('cannot read ${path}: ${err.msg()}')
    }
    ...
}
```

The rule that catches people: inside `or { ... }` on a **result**, the block must
produce that result's error value, so it is `return error(...)` and not a bare
`return`. On an **option**, `or { default }` supplies a value.

**Default**: return `!T` for anything that touches the outside world, and `?T` for
a lookup that is legitimately absent. Full detail, including how to propagate and
when a default is honest, in `references/OPTION-RESULT.md`.

## Sum types and match

V enums plus `match` are the way to model a closed set. `match` over an enum is
checked for exhaustiveness, which is the point: adding a variant turns every
incomplete `match` into a compile error instead of a silent fallthrough.

The diagnostic is exact: `non-exhaustive match expression without `else``. Two
ways out — handle every variant, or add an `else` branch and accept that a new
variant changes nothing.

```v ignore
enum Status {
    pending
    running
    done
    failed
}

fn label(s Status) string {
    return match s {
        .pending { 'queued' }
        .running { 'in progress' }
        .done { 'finished' }
        .failed { 'could not finish' }
    }
}
```

Variants are shared: `type Job = Status | string` is a sum type, and a `match`
over it must handle both sides. See `references/SUMTYPES.md`.

## mut is not optional

Function arguments are immutable. A receiver is immutable unless it is declared
`mut`. This is the single most common V compile error, and it always has the same
cause: the author assumed mutation was the default.

```v ignore
// Rejected: `app` is immutable.
// fn serve(app App) { app.port = 8080 }

// Accepted: the argument is mutable.
fn serve(mut app App) { app.port = 8080 }

// Accepted, and it reaches the caller: the receiver is a pointer.
fn serve(app &App) { app.port = 8080 }

// Accepted: the field itself is declared mutable.
struct Server {
mut:
    port int
}
```

`mut` on a **value** receiver mutates a copy, so the caller's struct is
unchanged. `mut` on a **pointer** receiver mutates the caller's. That difference
is silent at the call site, which is why `references/MUTABILITY.md` exists.

## Module names must match directories

`module foo` must live in a directory named `foo`. A mismatch imports cleanly and
then resolves to nothing, with no error pointing at the name. The `module` line
carries no hierarchy — the directory nesting supplies it.

This is why a project built by `v new` has `src/main.v` with `module main` rather
than `module src`.

## Comptime is `$`, and it excludes branches

Anything starting with `$` runs while the compiler runs. The branches of an
`$if` are **excluded entirely** on a platform that does not match, which is the
only reason a platform-specific import inside one is safe.

```v ignore
$if windows {
    import sys.windows as wsys
}
$if linux {
    import sys.linux
}
```

A runtime `if` that mentions a platform-specific symbol will not compile on the
other platforms, because there is no exclusion. Available in `$if`: `windows`,
`linux`, `macos`, `js`, `freebsd`, `android`, `debug`, `prod`, and your own
`$d custom_flag ? { ... }` — note the `?`.

## No implicit conversions

`int` is 32 bit and `i64` is 64. `f64` and `int` do not mix. Nothing converts
implicitly, which is deliberate: a silent narrowing is a bug you find later.

Be explicit at the boundary, and prefer `strconv` over hand-rolled parsing.

## Validation

After changing V code, prove it rather than reading it again:

```bash
v -check path/to/file.v        # type-check only, no binary
v fmt -verify path/to/file.v  # would the formatter change it?
```

For library code rather than a `main` module, `-check` alone fails with *"project
must include a `main` module"*; use `v -check -shared path/to/file.v`.

Run both before you report the change done. See `v-workflow` for the full loop.

## Related Skills

- **The build and test loop**: see [v-workflow](../v-workflow/SKILL.md) for
  `v.mod`, dependencies, when `./v self` is required, and the flags that go
  before the subcommand.
- **Writing tests**: see [v-testing](../v-testing/SKILL.md) when the change needs
  a `_test.v` file, or when `?T` and `!T` need asserting.
- **Asking the compiler**: see [v-mcp](../v-mcp/SKILL.md) when you want the
  declarations, references or diagnostics of a file without reading it, or when
  you want to edit through an AST-aware rename.
- **Memory and GC**: see [v-memory](../v-memory/SKILL.md) for GC modes, ownership
  checking and `unsafe`.