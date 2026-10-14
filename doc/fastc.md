# FastC backend

Select FastC with `v -b fastc run program.v`. It reads scanner tokens and emits C directly,
without the regular parser, checker, or AST pipeline, then compiles with bundled TinyCC.
FastC is experimental and supports a subset of V. An unsupported construct produces an error
with its source byte offset; FastC does not silently switch to the C backend.

Ordinary programs support the following constructs:

- Functions, primitive parameters and return values, local variables, constants, and explicit
  global variables enabled with `-enable-globals`.
- Integer, boolean, and string expressions; string interpolation; `print` and `println` for
  integers, booleans, strings, and enums.
- `if`, `match`, loops, `break`, `continue`, function calls, and selected compile-time conditions.
- Dynamic array literals such as `[1, 2, 3]` and `['a', 'b']`, with checked element access.
- Map literals such as `{'a': 1}` with string, integer, or floating point keys, element lookup,
  and `.len`.
  Lookup of an absent key returns the value type's zero value.
- Array and map element reads in local initializers and function arguments.
- `in` and `!in` for arrays, map keys, and substrings, including their use in boolean conditions.

For example:

```v
fn main() {
	values := [1, 2, 3]
	counts := {
		'apple': 2
		'pear':  1
	}
	println(2 in values)
	println(counts['apple'])
	if 'pear' in counts {
		println('found')
	}
}
```

This is a capability overview, not a guarantee that every combination of these constructs
works. Ordinary programs still reject division and modulo, shifts, `sizeof`, and several
runtime features. Floating point printing and most standard library imports are unsupported.
The self-hosting compiler path has additional internal lowerings and a different runtime;
its capabilities do not imply support in ordinary programs. Use the default C backend when
a program requires the full language or standard library.

For a project split across source files, pass its directory:

```sh
v -b fastc -enable-globals -o program path/to/project
```

Directory entries select the full builtin runtime, including V strings, maps, errors, and
garbage collection. Generic calls infer their concrete type from a mutable variable argument.
If a call reaches a generic placeholder without a concrete instance, it reports the function
and exits with an error instead of returning a zero value.
This path supports standard-library imports such as `os` and `toml`,
composite sum-type tests, integer and floating point interpolation formats, anonymous functions,
and immutable closure captures. Float formats support precision, width, left alignment, and the
`f` suffix to retain trailing zeros. A single-file entry can select the same runtime with
`-d fastc_real_builtin`; its module siblings are included.
A test file entry (`foo_test.v`, `foo_test.c.v`, or an architecture-qualified one) always uses
it, and builds as a program that runs its tests in source order between `testsuite_begin` and
`testsuite_end`, each between `before_each` and `after_each`; a failed assert or a returned error
fails that test, and the program exits with status 1 when any test failed. Tests and hooks require
zero parameters. Failed assertions run the test's deferred statements before the harness continues.
Target-specific files and test files are filtered when loading a directory.
Generic arguments infer the complete type of literal arithmetic, including float promotion.
Other compound arguments require explicit type arguments.
Bare `nil` can reset concrete `voidptr` locals, including aliases, and declared `voidptr`
fields. It can initialize declared `voidptr` fields and serve as an argument to `voidptr`
and function-typed parameters. Function fields and erased generic fields require `unsafe`.
Channel type declarations accept ordinary V whitespace.
Negated sum-type guards retain pointer identity and read mutable scalar variants as values
in arithmetic, including numeric aliases.
Fixed arrays and their aliases support checked indexing, including pointer elements.
Pointer elements retain their identity when passed or copied. Omitted option fields are `none`.
Sum-type and interface tests narrow later operands of a conjunction, preserving pointer values.

Capturing callbacks proven local to a block release their runtime slot and captured context
when the block exits, including through `return`, `break`, and `continue`. Callbacks that escape
or have unproven ownership remain managed by the closure runtime.
Blank imports retain their native inputs for initialization side effects. Native binary inputs
are also retained when their side effects cannot be proven unused. Executable link caching is
disabled whenever native source or binary inputs are present, so replacing an input at the same
path takes effect on the next build.

Native flags support `$first_existing(...)` and `$when_first_existing(...)`, including
library inputs and include/search path prefixes. They select the first existing candidate;
an unmatched `$when_first_existing` omits the flag, while `$first_existing` reports an error.
Quoted candidate filenames can contain spaces and commas.
This includes the optional atomic-library flags used on Linux.
Linux GC builds load atomic support even without an explicit `sync` import. On Linux amd64
with musl, TinyCC's helpers and the driver-prepared fence object provide this support.
Native C helpers are linked before the static archives they call, including the bundled GC.
Under TinyCC on macOS and Linux, preallocation uses pthread storage to keep each thread's
arena independent. The required pthread library is linked even without `spawn`.
Dynamic array literals with complex element expressions evaluate those elements before copying
the array, avoiding nested C aggregate initializers unsupported by bundled Linux TinyCC.
Their temporaries avoid collisions with local and parameter names.
Channels on this path use the standard channel runtime for buffered and unbuffered
sends and receives, `select`, closing, and channel properties.
Spawned threads and arrays of threads support waiting for void and typed results.
`nil` can reset `voidptr` fields and locals. Typed pointer `nil` literals still
require an `unsafe` block.

The full-runtime path remains experimental. Mutable and expression closure captures and
128-bit integers are unsupported. Disabled standalone `@[if flag ?]` calls omit their arguments.

Build the full V compiler, including its regular C backend, directly from `cmd/v`:

```sh
v -b fastc -nocache -o v_fastc1 cmd/v
./v_fastc1 -b fastc -nocache -o v_fastc2 cmd/v
./v_fastc2 -b fastc -nocache -o v_fastc3 cmd/v
./v_fastc3 -b c run examples/hello_world.v
```

Each generation retains the full command-line entry and can compile programs with either
the C or FastC backend. `-nocache` forces each generation to rebuild its generated C units.

`v -b fastc self` builds a smaller standalone FastC compiler from `vlib/v/v.v`.
Its `self` command keeps FastC for subsequent generations; use `v -b fastc self x2` to
check repeated replacement builds. This standalone compiler only supports the FastC backend.
Standalone replacements accept directory entries and `-enable-globals`. Their `-nocache`
option disables TinyCC object and executable caching, including in repeated self-builds.
The launcher helper runs on the host C backend. Native source flags are retained during
self-builds, and a missing `.o` input is compiled from its matching `.c` source when available.
Use `-keepc` to retain generated C, including when TinyCC rejects it.
