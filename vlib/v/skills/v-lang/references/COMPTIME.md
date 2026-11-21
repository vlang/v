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
| `$res` | embed an Android resource |
| `$pkgconfig` | query pkg-config |
| `$typeof`, `$sizeof`, `$isreftype`, `$__offsetof`, `$dump` | aliases of `typeof`, `sizeof`, `isreftype`, `__offsetof`, `dump`; prefer those |

## Built-in `$if` conditions

`windows`, `linux`, `macos`, `freebsd`, `openbsd`, `netbsd`, `dragonfly`, `android`,
`ios`, `js`, `wasm32`, `tinyc`, `gc_boehm`, `gcc`, `clang`, `msvc`, `tcc`,
`debug`, `prod`, and their negations with `!`.

## Custom flags

A flag you define needs the `?`. Without it, `$if my_feature {` reads the name as
an identifier: it works while you build with `-d my_feature`, and the build fails
with `undefined ident: my_feature` as soon as someone builds without it.

```v ignore
// Enabled by: v -d my_feature
$if my_feature ? {
    import mycompany.feature
}
```

The standard library uses this throughout, for example `$if trace_ssl ?` and
`$if ownership ?`.

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

`$embed_file` returns an `EmbedFileData`, not a string: call `.to_string()` or
`.to_bytes()` for the contents. With `-prod` the bytes are inlined into the binary.
Without it, the program reads the file from disk when the contents are first
asked for, so a development build fails at run time if the file has moved. This
bites people who test one build and ship the other.

```v ignore
const help = $embed_file('help.txt')

fn main() {
	println(help.to_string())
}
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