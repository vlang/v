# Comptime in depth

> The rule that branches are excluded is in the parent skill. This reference
> covers: every supported `$` form, custom flags, reflection, and embed files.

Anything beginning with `$` is evaluated while the compiler runs, not while the
program runs. The `$if` branches are **removed from the generated C entirely** on
a platform that does not match, which is the only reason a platform-specific
import inside one is safe.

## The forms

| Form | What it does |
| --- | --- |
| `$if` | choose code at compile time |
| `$for` | unroll a loop at compile time |
| `$compile_error` | stop the build with a message |
| `$compile_warn` | report without stopping |
| `$embed_file` | inline a file's bytes or text |
| `$tmpl` | render a template |
| `$env` | read an environment variable |
| `$d` | read a `-d name=value` define |
| `$typeof`, `$sizeof`, `$isref`, `$isreftype`, `$isnil`, `$dump` | introspection |

## Built-in `$if` conditions

`windows`, `linux`, `macos`, `freebsd`, `openbsd`, `netbsd`, `dragonfly`, `android`,
`ios`, `js`, `wasm32`, `tinyc`, `gc_boehm`, `gcc`, `clang`, `msvc`, `tcc`,
`debug`, `prod`, and their negations with `!`.

## Custom flags

A flag you define needs the `?`. Without it, `$if` expects a builtin and a custom
name silently fails to match — so your branch is dropped without a word.

```v ignore
// Enabled by: v -d my_feature
$if my_feature ? {
    import mycompany.feature
}
```

The V repo itself uses this: `v -d trace_checker`, `v -d v3`, `v -d tinyc`.

## Imports belong inside the $if

This is the whole trick:

```v ignore
$if windows {
    import sys.windows as wsys
}
$if linux {
    import sys.linux
}
```

A runtime `if` cannot do this, because the symbol is resolved before the branch
runs:

```v ignore
// Does not compile on Linux: wsys does not exist there.
if os.user_os() == 'windows' {
    wsys.something()
}
```

## $for is unrolled

There is no loop variable at runtime. Each iteration is its own code, so `$for` is
for generating repeated declarations, not for iterating a runtime collection:

```v ignore
$for field in Config.fields {
    $compile_error('Config needs a field named ${field.name}')
}
```

For reflection over types at runtime there is none — that is what `$for` is for.
See `v-memory` for the `unsafe` escape hatch when you genuinely need a type
parameter.

## $embed_file

`$embed_file` inlines a file as a compile-time constant. Without `-prod` it
inlines the **path**, not the contents; with `-prod` it inlines the bytes. This is
a documented difference and it bites people who test a build and ship a different
one.

```v ignore
// -prod only: the contents
const help_text = $embed_file('help.txt')

// Always: the path
const help_path = $embed_file('help.txt')
```

## Verification

A comptime mistake surfaces at build time, so the checker is the test:

```bash
v -check path/to/file.v
```

For a conditional import, check the branch you are not building too, by passing
the opposite flag — otherwise only one half of the code is ever compiled:

```bash
v -check -d my_feature path/to/file.v
```