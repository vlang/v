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
garbage collection. This path supports standard-library imports such as `os` and `toml`,
composite sum-type tests, integer interpolation formats, anonymous functions, and immutable
closure captures. A single-file entry can select the same runtime with `-d fastc_real_builtin`;
its module siblings are included.
Target-specific files and test files are filtered when loading a directory.

The full-runtime path remains experimental. Mutable and expression closure captures and
128-bit integers are unsupported. Disabled standalone `@[if flag ?]` calls omit their arguments.

Build a FastC compiler with `v -b fastc self`. Its `self` command keeps FastC for subsequent
generations; use `v -b fastc self x2` to check repeated replacement builds.
Standalone replacements accept directory entries and `-enable-globals`. Their `-nocache`
option disables TinyCC object and executable caching, including in repeated self-builds.
The launcher helper runs on the host C backend. Native source flags are retained during
self-builds, and a missing `.o` input is compiled from its matching `.c` source when available.
Use `-keepc` to retain generated C, including when TinyCC rejects it.
