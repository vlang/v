# Releasing a V module as a C library

`v c-api` generates a C ABI surface from the public declarations of a V module.
No `@[export]` attribute or hand-written wrapper is needed: every `pub fn`,
`pub method`, `pub const`, `pub struct` and `pub enum` is considered.

## Usage

```sh
# generate artifacts into dist/ and build the shared library
v c-api ./mymodule -o dist --lib-name mylib --version 1.2.3

# the same thing as a flag on a shared build (-shared is implied)
v -c-api ./mymodule --c-api-dir dist -o dist/libmylib.so
```

Use `--prod` for release builds. It is required when a module uses
`$embed_file`: debug builds keep the source path and fail at runtime once the
temporary build tree has been removed; production builds embed the bytes.

Generated files:

- `dist/libmylib.so` (or `mylib.dll` / `libmylib.dylib`)
- `dist/mylib.h` — C declarations, opaque struct handles, enum values
- `dist/mylib.def` — Windows DLL export list
- `dist/mylib.pc` — pkg-config metadata
- `dist/demo.c` — minimal link check
- `dist/README.md` — consumer-facing summary

The module directory is compiled as-is; two temporary generated files
(`capi_generated.v` and `capi_generated.c.v`) are created there and removed
after the build. Pass `--keep-generated` to inspect them.

## Type mapping

| V | C parameter | C return |
| --- | --- | --- |
| `bool` | `int` (0/1) | `int` |
| integer and float primitives | matching `stdint.h` type | same |
| `string` | `const char *` | `char *` (caller frees) |
| `[]T` (numeric) | `const T *items, int items_len` | `T **out, int *out_len` via `int` status |
| `![]T` / `?[]T` | — | `T **out, int *out_len` via `int` status |
| `!T` | `T *out` | `int` status (0 ok, -1 error) |
| `?T` | `T *out` | `int` status (0 some, 1 none) |
| `&PubStruct` | opaque struct pointer | same |
| scalar-only `pub struct` by value | generated C struct passed by value | same |
| `pub enum` | `int` / generated C enum | same |
| `pub const` | generated `<prefix>_get_<name>()` getter | scalar or string |

Methods with a pointer receiver on a public struct become
`<prefix>_<Type>_<method>(<prefix>_Type *self, ...)`.

Structs are emitted as opaque handles unless every field is a scalar (integer,
float or bool); scalar-only structs get a C struct definition with the same
layout and can be passed or returned by value. Structs containing strings,
slices or nested structs are skipped with a warning.

Generics, maps, channels and closures are skipped with a warning rather than
exported with a V-internal ABI.

## Memory and errors

Strings and slice buffers returned by the library are C heap copies. Release
them with `<prefix>_free()`. Error text from a failed `!T` call is available from
`<prefix>_last_error()` and stays valid until the next call on that thread.
Call `<prefix>_init()` once before using the library.

## Packages with submodules

When the target directory contains subdirectories with their own `module`
declarations, all of them are included in one shared library. The import path
is read from `v.mod` (`name` field); override it with `--import-path`, e.g.:

```sh
v c-api . --lib-name vtls --import-path metif12.vtls -o dist
```

Each submodule's functions get a `<lib>_<submodule>_<fn>` C name to avoid
collisions. Use `--no-submodules` to build only the module in the given
directory. During the build a temporary `VMODULES` tree is created under the
system temp directory and removed afterwards (`--keep-generated` retains it).

## Manual control

Automatic export is the default, but three attributes let a module adjust the
surface without writing wrappers:

Import `v.capi` and use these attributes:

- `@[capi.CApiSkip]` on a `pub fn` leaves it out of the C surface.
- `@[capi.CApiName('custom_name')]` overrides the generated C symbol name.
- `@[capi.CApiExport]` includes a private `fn` in the C surface.
- `@[capi.CApiErrno(7)]` returns status `7` when a `!T` function fails
  (default is `-1`); the error text is still available from `<prefix>_last_error()`.

The bare spellings (`@[c_api_skip]`, etc.) are also recognised by the generator
but produce an unknown-attribute warning from the checker; prefer the qualified
forms.

## Limitations (v1)

- Only one module directory per invocation; submodules are not traversed.
- A `!T` function has one failure status per function, not per error value.
- Struct fields are not exposed; use V functions as factories and accessors.
