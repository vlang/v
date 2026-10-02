# Options and Results in depth

> The decision table is in the parent skill. This reference covers: the four
> unwrapping forms, propagating an error, and when a default value is honest.

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

## The four unwrapping forms

```v ignore
// 1. Propagate. The block produces the error value, so it ends in `error(...)`.
//    `err` and `err.msg()` are in scope inside the block.
port := os.read_port(path) or { return error('cannot read ${path}: ${err.msg()}') }

// 2. Handle. Do the work here and continue.
body := os.read_file(path) or {
    eprintln('skipping ${path}')
    return
}

// 3. Abort. Only where a value is needed unconditionally, and never in a library
//    that a caller depends on.
body := os.read_file(path) or { panic(err) }

// 4. Default. For an option only. For a result the block must return that result's
//    error type, so `or { 0 }` does not compile there.
first := list.first()
```

## Propagating without adding noise

`or { return err }` is correct and ugly when the error already says everything.
When it does not, add what the caller cannot see:

```v ignore
// Not useful: the message already names the file.
f(path) or { return err }

// Useful: the caller does not know which of three files failed.
f(path) or { return error('config: ${err.msg()}') }
```

## `?T` returning `!T`

An option-returning function cannot be handed to a `!T` caller directly, and
neither shape converts implicitly. Make the boundary explicit:

```v ignore
// A lookup that is absent is an error here, because this caller requires a value.
port := find_port(cfg) or { return error('no port configured') }
```

## Smart casts

Inside `if x != none`, V narrows `x`, so the unwrapped value is available without
a second unwrap:

```v ignore
if cfg.port != none {
    println(cfg.port)   // int, not ?int
}
```

An `if` **guard** does the same and skips the body entirely when there is no
value:

```v ignore
if port := cfg.port {
    start(port)         // only reached when there is a port
}
```

This is the idiomatic form. Prefer it over `!= none` when you do not need the
value afterwards, because it cannot be forgotten.

## Rules that are not negotiable

- A function returning `!void` still needs `return error(...)`; there is no bare
  `return` that carries a reason.
- Do not return `0`, `''` or `-1` to mean failure. A caller cannot tell that apart
  from a real value, and neither can a test.
- Do not swallow an error with `_` unless a comment says why it is safe.

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