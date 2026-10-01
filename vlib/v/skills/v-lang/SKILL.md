---
name: v-lang
description: The V language rules agents most often get wrong, before writing V code.
---

# V rules worth knowing before writing V

Most V mistakes an agent makes are not algorithmic. They are these rules.

## `?T` is not `!T`

`?T` is an option: it holds a value or `none`. `!T` is a result: it holds a value
or an error. They are unwrapped differently, and mixing them up does not always
look like a type error.

```v ignore
// Option
config := load_config(path) or { return }
if config != none {
    println(config.port)  // `config` smart-casts to Config here
}

// Result
text := read_file(path) or { return error('cannot read ${path}') }
// read_file returns !string, so the error carries the message
```

- `or { ... }` unwraps a **result**; the block must return that result's error
  type, so it is usually `return error(...)`.
- `or { Config{} }` supplies a default for an **option**.
- A function that returns `!void` still needs `return error('...')`.
- `x or { panic(err) }` only makes sense where a value is needed unconditionally.
  Inside a library, propagate the error instead.

Unwrapping inside an `if` guard is a common trap:

```v ignore
if x := maybe_value() {
    // only reached when there is a value
}
```

## Comptime is `$`, not `$if` at runtime

Anything starting with `$` runs while the compiler runs. A runtime `if` that
mentions a platform specific symbol will not compile on the other platforms.

```v ignore
$if windows {
    import sys.windows as wsys
}
$if linux {
    import sys.linux
}
```

Supported compile-time forms: `$if`, `$for`, `$compile_error`, `$compile_warn`,
`$embed_file`, `$tmpl`, `$env`, `$d`. The `$if` branches are excluded entirely on
a platform that does not match, which is what makes them safe.

Available in `$if`: `windows`, `linux`, `macos`, `js`, `freebsd`, `android`,
`debug`, `prod`, and your own `$d custom_flag ? { ... }` with the `?`.

## Immutability

Function arguments are immutable. Add `mut` to change one:

```v ignore
fn build(mut app &App) {
    app.port = 8080  // mut receiver
}
```

Struct fields declared `mut:` can be assigned after construction. A `mut`
receiver of a pointer type (`mut app &App`) mutates the caller's object rather
than a copy.

## Module names must match directories

`module foo` must live in a directory named `foo`. A mismatch imports cleanly and
then fails to find anything, with no error pointing at the name. The `module` line
carries no hierarchy: the directory nesting supplies it.

## Structs and maps

Use a struct when the fields are known and named; use a map when they are dynamic
keys. A `map[string]string` for a fixed set of five fields costs a lookup per
access and loses the field names at every call site. Maps in V are not ordered;
sort the keys when output order matters.

## No implicit conversions

There are no implicit numeric conversions, and `int` is 32 bit while `i64` is 64.
`f64` and `int` do not mix. Be explicit at the boundary.

## Errors and panics

Return `!T` and propagate. Reserve `panic` for a program that cannot continue,
and `assert` for an invariant the compiler cannot check. `eprintln` writes to
stderr, which is what a tool should use for diagnostics that are not its answer.

## Formatting

Run `v fmt` (or the `v_format` MCP tool) on every file you touch. The formatter is
strict: a file it reformats does not match the one you wrote, so format before you
finish rather than after.