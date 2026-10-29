# FastC backend

Select FastC with `v -b fastc run program.v`. It reads scanner tokens and emits C directly,
without the regular parser, checker, or AST pipeline, then compiles with bundled TinyCC.
FastC is experimental and supports a subset of V. An unsupported construct produces an error
with its source byte offset; FastC does not silently switch to the C backend.

Ordinary `v self` builds include only the C backend. The first `-b fastc` request automatically
builds a full compiler in the tool cache and runs it; subsequent requests reuse that compiler.
Use `v -compile-backend fastc self` to include FastC in the main executable instead.

On Windows, ordinary FastC programs set stdout and stderr to binary mode, preserving `\n`
without CRT conversion to `\r\n`, as the default C backend does.

Sum types currently use boxed payloads in FastC. Copying a sum containing a value variant can
therefore share that variant's storage, unlike the independent value copies in the default
C backend. Programs relying on sum value snapshots should use the default C backend until
FastC implements the same copy semantics. Reference-variant support is also incomplete.

As in the default C backend, an `if` condition cannot start with another `if` expression,
even behind opening parentheses. Assign the inner expression to a variable first.
Both the C emitter and the native ARM64 parser enforce this rule.
The condition diagnostic preserves original source-file byte offsets through generic rewrites,
selected compile-time branches, and declaration initializers. The C emitter also preserves these
positions when it reparses function literals, substituted compile-time loop bodies, or
inline-assembly operands, including comment removal and nested rewrites. The common diagnostics
for unsupported constructs do the same in both parsers.
The C emitter also maps scanner errors in struct-field defaults.

Files marked `@[translated]` retain support for these conditions, including constant, global, and
struct-field initializers. On-demand generic instances use their defining file's translated status
and source positions.

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
Mutable field arguments also retain their type when inferring a generic instance.
Empty map initializers use their expected key and value storage, including map aliases.
On glibc Linux, thread IDs use the system call without requiring glibc 2.30. Generic
parameter shadowing ends with its function and does not affect later constants.
Function-pointer fields and parameters support mixed named and unnamed parameter types.
If a call reaches a generic placeholder without a concrete instance, it reports the function
and exits with an error instead of returning a zero value.
This path supports standard-library imports such as `os` and `toml`,
composite sum-type tests, integer and floating point interpolation formats, anonymous functions,
and immutable closure captures. Float formats support precision, width, left alignment, and the
`f` suffix to retain trailing zeros. A single-file entry can select the same runtime with
`-d fastc_real_builtin`. As with the C backend, a program file named on its own is compiled
alone, while a test file also brings in the other files of its module.
A test file entry (`foo_test.v`, `foo_test.c.v`, or an architecture-qualified one) always uses
it, and builds as a program that runs its tests in source order between `testsuite_begin` and
`testsuite_end`, each between `before_each` and `after_each`; a failed assert or a returned error
fails that test, and the program exits with status 1 when any test failed. Tests and hooks require
zero parameters. Failed assertions run the test's deferred statements before the harness continues.
Target-specific files and test files are filtered when loading a directory.
Dependency files retain imports after declarations, including qualified type names.
Generic arguments infer the complete type of literal arithmetic, including float promotion.
Other compound arguments require explicit type arguments.
Bare `nil` uses concrete expected types for `voidptr` locals and aliases, fields and their
defaults, wrapped and multi-value returns, both sides of comparisons, and array elements.
Calls accept parenthesized `nil` for
`voidptr` parameters, including methods and variadic parameters. Typed pointer arguments also
accept `nil` in `.c.v` files. Function storage and erased generic types still require `unsafe`.
Channel type declarations accept ordinary V whitespace.
Negated sum-type guards retain pointer identity and read mutable scalar variants as values
in arithmetic, including numeric aliases.
Fixed arrays and their aliases support checked indexing, including pointer elements.
Pointer elements retain their identity when passed or copied. Omitted option fields are `none`.
Dynamic pointer arrays use pointer identity for `.index()` and `.last_index()` searches.
Explicit integer and pointer casts preserve pointer values, including through pointer aliases.
Implicit `voidptr` conversions to typed pointers also retain the address when assigned or passed.
Fixed-array dimensions accept integer constant arithmetic. Globals and native array fields retain
their original storage when indexed, assigned, or passed to memory functions.
Nested fixed-array initializers retain their dimensions and element layout. Iteration over fixed
array literals uses their element storage; integer arrays and pointers retain FastC's target width.
Arrays of anonymous functions retain their parameter and return types when indexed and called.
Membership tests compare fixed-array elements by value. Single-file bootstrap programs apply
struct field defaults to empty initializers.
Promoted fields in pointer embeds support scalar and fixed-array overrides. Overrides copy the
embedded value, preserving shared defaults and unchanged fields from struct updates. A nil embed
uses the embedded type's field defaults before applying overrides.
Sum-type and interface tests narrow later operands of a conjunction, preserving pointer values.
Generic pointer instances retain pointer depth and mutable storage, including forwarded references.
`sizeof(int)` uses FastC's target integer width. Returned, global, and struct-field callbacks retain
their argument and Option/Result signatures through inferred and copied locals.
Calls through function-alias casts retain the callback's argument and return signatures.
Fixed and dynamic arrays of callbacks retain those signatures when indexed, including
Option/Result returns. Generic pointer arguments accept transparent `unsafe` expressions.
Global arrays support appends, indexing, and mutable
iteration across modules while retaining their original storage.
Appending a borrowed string variable, field, or element copies its bytes so the array keeps
its value after the source is freed. Call results and bulk appends retain their existing storage.
Module cycles retain every source and run each initialization and cleanup hook once in stable order.
By-value parameters whose addresses escape to spawned workers retain shared storage after the
function returns. Later writes and workers from the same invocation use that storage.
Escaped keyword fields and locals, and the ESC escape `\e` in runes and strings, are supported.
Multiline values can continue with leading binary operators or a following `else` branch.
Postfix updates used as values return the old value, and trailing variable attributes preserve
the following statement. Option/Result tuple fallbacks can return an enclosing function's tuple.
Grouped Option/Result propagation preserves payload types and early returns, including in call
arguments.
Optional sum-type lookups preserve `none` fallbacks and successful payloads.
Typed pointers to `IError` implementations can be successful `voidptr` Result payloads.
Direct integer tuple returns store each component at its declared width, including literal values.
Assertions preserve the following fallback statement when it begins with a unary operator.
Standalone `unsafe` blocks preserve local scopes. Forward labels remain reachable after a return
in translated code.

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
Native C helpers are compiled before the static archives they call, including the bundled GC,
for both serial builds and parallel builds using macOS's in-process linker.
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

Cross-target builds can emit C for an external compiler with `-o program.c`. This mode accepts
`-prod`; the external compiler chooses the C optimization level. For freestanding targets,
`-target-libc-headers` uses the target's core C headers and allocator. It avoids hosted startup
and thread-local allocation setup. The target calls `C._vinit(0, unsafe { nil })` after its
allocator is ready to initialize V constants, globals, and module hooks.
Numeric, null, and constant aggregate global initializers use static storage, so early native
code can read and change them before `_vinit`; runtime initialization preserves those changes.
Dynamic initializers and references to other globals or functions remain deferred until the
target calls `_vinit`, unless explicitly marked `@[cinit]`.

This path preserves module-qualified entry names and `@[export: 'name']` native symbols.
Exports with a corresponding C declaration use its integer ABI while internal V calls retain
V's integer width. Grouped globals support typed callbacks and `volatile` storage. `@[cinit]`
globals have static initializers, including fixed-array struct defaults; linker sections,
packed structs, aligned structs, and volatile fields retain their C layout and qualifiers.
Local volatile declarations retain their storage qualifiers. Interface fields refer to the
concrete object's storage, including chained mutations and mutable arguments.
Mutable interfaces dispatch to compatible value-receiver and pointer-receiver implementations.
Architecture-filtered source trees keep their selected siblings. An explicitly imported module
missing from a staged tree can resolve beside the original source files.
The target runtime emits native prototypes for referenced C declarations in V files without
an active C include. Files that include C headers leave their native declarations to the C
compiler. FastC does not read or parse C headers; their macros, transitive includes, and
conditional declarations are handled by the target C compiler.
An explicit `@[c_extern]` declaration still requests a prototype from its V signature.
C variadic declarations retain their native calling convention. Exported and `@[markused]`
functions retain their reachable private helpers. An explicit C binding to a uniquely matching
marked V function can retain its native module-basename name and declared C ABI.
Functions returning two `u64` values use a native pair-return adapter in explicit casts to
`voidptr` on this target path, including syscall tables called by assembly. Direct V calls and
typed V callbacks retain their internal tuple representation.
The runtime omits hosted compatibility headers,
signal-handler setup, and inactive preallocation support.
Small Option and Result payloads use storage inside the returned value on this target path.
Copies retain their payload without allocating, allowing allocator helpers to return optional
pointers without reentering the allocator. Larger payloads retain allocated storage.

Translated code on this freestanding full-runtime path supports integer division and modulo,
including compound assignments. Bare `int` and its aliases retain the target's integer width,
while explicitly sized integer types retain their declared width.
Operands and assignment targets are evaluated once; zero divisors panic.
Signed 32-bit and 64-bit minimum values divided or reduced modulo `-1` also panic.
Hosted translated code accepts proven positive constant divisors and keeps rejecting unproven
divisors. The ordinary FastC subset retains its division and modulo restrictions.

AMD64 and ARM64 C output supports GNU inline assembly, including named operands, read/write
constraints, clobbers, local labels, raw templates, and `asm goto`. Structured Intel assembly
supports register, immediate, and memory operands, including memory read/write constraints.
Compile and link that output with the target toolchain; FastC executable builds still use the host
target and bundled TinyCC.

Build the full V compiler, including its regular C backend, directly from `cmd/v`:

```sh
v -b fastc -nocache -o v_fastc1 cmd/v
./v_fastc1 -b fastc -nocache -o v_fastc2 cmd/v
./v_fastc2 -b fastc -nocache -o v_fastc3 cmd/v
./v_fastc3 -b c run examples/hello_world.v
```

Each generation retains the full command-line entry and can compile programs with either
the C or FastC backend. `-nocache` forces each generation to rebuild its generated C units.
Clang production self-builds also exercise this full-compiler FastC path during PGO training
when FastC is included. Compiler sources must stay within the FastC self-hosting subset for
that training to complete.
This path preserves enum shorthand types in parallel assignments and imported call arguments,
and lowers flag enum `.has(flag)` calls on variables and imported function results.
On macOS ARM64, standard full-compiler builds compile the owned native support files together
so TinyCC processes shared SDK declarations once. Debug builds, custom preprocessing flags,
and other native source sets retain separate compilation. FastC does not read their C headers.

`v -b fastc self` builds a smaller standalone FastC compiler from `vlib/v/v.v`.
Its `self` command keeps FastC for subsequent generations; use `v -b fastc self x2` to
check repeated replacement builds. This standalone compiler only supports the FastC backend.
Standalone replacements accept directory entries and `-enable-globals`. Their `-nocache`
option disables TinyCC object and executable caching, including in repeated self-builds.
The launcher helper runs on the host C backend. Native source flags are retained during
self-builds, and a missing `.o` input is compiled from its matching `.c` source when available.
Use `-keepc` to retain generated C, including when TinyCC rejects it.

For throughput measurements, first build a production compiler with `v -prod -cc clang self`.
On macOS, measure a complete uncached executable build with an external timer:

```sh
FASTC_BENCH=1 /usr/bin/time -l v -b fastc -nocache -o v_fastc_bench cmd/v
```

Count physical lines in the selected V source files, including blank lines and comments.
Divide that count by the external elapsed time to include compiler startup, C generation,
TinyCC compilation, linking, signing, retained C output, and process exit. The internal
`FASTC_BENCH` total omits early command-line processing and final output publication.
Generated C lines and C header lines are not V source lines. FastC does not scan C headers.

Keep the source snapshot, compiler build options, toolchain, worker count, and output paths
fixed when comparing revisions. Warm both compiler executables before timing; on macOS,
replacing an executable can add first-execution validation time. Then retain two warmups
and ten alternating baseline/candidate pairs. Compare retired instructions, elapsed time,
and peak memory, and verify that both revisions generate equivalent C. Record whether
third-party helper objects were already built; `-nocache` rebuilds generated V units but
does not remove those toolchain dependencies.
