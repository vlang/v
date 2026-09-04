# v3

Clean rewrite of the V compiler. Reuses v2's scanner, uses a flat AST parser
with Pratt parsing, a structured type system with sum-type variants, lexical
scoping, a transformer for AST simplification, a shared type-checking phase, a
markused pass for dead-code elimination, recursive import resolution, and
backends: a direct flat-AST-to-C backend, a scanner-to-C fast path, a native
ARM64 backend via SSA IR with a built-in linker, and a direct
flat-AST-to-WebAssembly backend. With `-prod`, the ARM64 backend runs SSA
optimization, MIR lowering, and instruction selection.

The `v fmt` command uses `v3.parser` and `v3.gen.v`. Formatter-mode parsing retains comments,
compile-time branches, inline assembly, SQL bodies, and literal prefixes so they round-trip
without a legacy formatter path.

Imports all `vlib/builtin/` V source files, both pure V (`.v`) and C-interop
(`.c.v`), for struct, enum, type alias, interface, C function declarations, and
global definitions. `$if` compile-time conditionals are resolved directly in the
parser. The parser evaluates the condition, parses only the taken branch, and
skips the other, so no AST nodes or transformer pass is needed for `$if` blocks.
`#include` and `#flag` directives inside `$if` blocks are handled correctly: the
scanner consumes the entire directive line as a single token, preventing the
parser from reading past block boundaries. File selection filters out
arch-specific files (`.arm64.v`, `.amd64.v`) against the selected target and deduplicates function
definitions when both `.v` and `.c.v` files exist. C runtime functions
(println, string ops, int_str, etc.) are still provided via a built-in preamble;
builtin function bodies are skipped during C code generation. Maps use the
builtin `map` type name and API (`new_map`, `map__set`, `map__get`,
`map__delete`, etc.) with a simplified open-addressing implementation until v3
can compile the full builtin map.v.

## macOS V3 dispatch

On macOS and Linux, V3 is the default compiler for user source and test builds. The top-level
`v` command runs the V3 driver linked into `cmd/v`; it does not build or launch a second compiler
process. This includes direct file and directory builds, `run`, `build`, and test-file compilation,
plus production and shared builds and supported cross targets and backends. The `test` command
itself continues to use the established test dispatcher, while each discovered test file is
compiled by V3.

`cmd/v` remains the CLI and compatibility dispatcher. Its own build, its internal command-tool
bootstrap, and the `vlib/v3/v3.v` compiler bootstrap retain the compatibility compiler. Explicit
non-none garbage collectors, sanitizer builds, live reload, and autofree also stay off the default
V3 path until V3 supports their runtime behavior. Debug builds selected with `-g`/`-debug` also use
the established compiler; `-cg`/`-cdebug` remains eligible for V3. Pass `-old-compiler` to
explicitly select the compatibility compiler for another user build. On Windows and the BSDs,
where the V3 driver is not embedded, `v` uses the established compiler by default.

Pass `-new-compiler` for the opposite: it runs the embedded V3 driver (`vlib/v3`) in the SAME
process, exactly like the default macOS and Linux path — it never launches a separate `v3`
executable. Because only macOS and Linux embed the V3 driver, `-new-compiler` applies there: it
forces V3 for a `run`/`build` target even when the default heuristic would defer to V1, and
disables the automatic V1 fallback so a V3 failure is reported instead of silently retried with
V1. It never forces V3 onto options V3 cannot honor yet (those error asking you to drop the flag),
leaves the `test` command to the test dispatcher, and errors on builds that do not embed the V3
compiler — Windows, the BSDs, and the portable cross-VC bootstrap — rather than opting them into
V3. `-old-compiler` takes precedence when both are given.

To make this possible the V3 driver (`vlib/v3`) is linked into `cmd/v` on macOS and Linux, so `v`
compiles in-process there by default and `v -new-compiler` does so wherever V3 is embedded. Windows
and the BSDs get a stub instead, so `-new-compiler` there reports that the build does not embed V3.

The in-process path supports the split module cache and uses parallel stages while the input
remains within its scratch-memory safety limit.

When delegated V3 compilation rejects a source before producing its output, `cmd/v` automatically
retries the command through the established compiler. Exit codes from successfully compiled
`run` programs are returned unchanged and do not trigger a retry.

When V3's generated C fails to compile, `cmd/v` automatically retries the command through the
established compiler. If that retry succeeds, the existing automatic C-error reporter submits the
V3 diagnostics to bugs.vlang.io with `V3` in the report's build options. The usual
`V_C_ERROR_BUG_REPORT_DISABLED` and GitHub CI safeguards still apply.

## Target selection

The C backend accepts `-os <name>` and `-arch <name>`. The target controls source-file suffix
selection, `$if` conditions, target-qualified `#flag` and `#include` directives, shared-library
suffixes, and third-party object-cache keys. Common aliases such as `darwin`, `x86_64`, and
`aarch64` are normalized. Native linking currently supports the host target and macOS
`amd64`/`arm64` cross-architecture builds through Clang's `-arch`; other cross targets can be
emitted as C with `-o file.c` for compilation by an external target toolchain.

The command line rejects unknown options, missing option values, unsupported backends, and
multiple input paths. `-cc <executable>` selects the C compiler and `-gc none` is the only
currently supported collector mode. Directory builds read `subdirs` through the canonical
`v.mod` parser, including when other manifest strings contain punctuation resembling fields.
Native C compilation uses `-fwrapv` on supported targets so signed integer overflow retains V's
two's-complement semantics. On macOS, `-cg` links executables with exported symbols for symbolic
backtraces while plain `-g` retains its V-source debug behavior.
For `-prod`, generated C units of at least 8 MiB use `-O2 -flto`; Clang also uses a bounded inline
threshold for those units. Smaller production units retain `-O3 -flto`.
The driver monitors compiler memory throughout the build. Ordinary builds stop at 10176 MiB;
compiler-tree and self-host builds stop at 9984 MiB, leaving extra sampling headroom below a
10 GiB process ceiling.
On macOS it uses physical footprint, matching Activity Monitor more closely; elsewhere it uses
current RSS. Pass `-no-memory-limit`/`--no-memory-limit` to disable this safety limit.
On macOS and Linux, `make` and the default `v self` build the compiler with `-prealloc`, enabling
the disposable stage arenas that keep compiler self-hosting within that ceiling.
Stage rows recorded at pipeline boundaries report sampled peak RSS and the process peak. Timing
breakdowns reconstructed after a stage omit the sampled peak. On macOS each row also prints
physical footprint immediately after RSS.

## Parallel jobs

`VJOBS` selects V3's desired parallel job count. On Linux, an executable of the V3 compiler that
was itself built with `-prealloc` caps ordinary user builds at four total compiler jobs whenever
the effective job count is greater than four. The caller thread counts as one job, so V3 creates
at most three worker threads. `VJOBS` values from 1 through 4 are unchanged; values greater than
4 cannot override this cap.

The cap does not apply to compiler/self-host inputs or to V3 executables built without
preallocation. It depends on how the V3 compiler executable was built, so passing `-no-prealloc`
for the user program being compiled does not disable the compiler's own job cap.

## Fast C backend

`-b fastc` selects the embedded V3 driver and its AST-free parser for the shortest edit-run cycle.
FastC resolves the entry file and imported modules, then emits GNU C while consuming scanner tokens.
It never invokes the flat parser, semantic checker, transformer, mark-used pass, or conventional C
generator. For same-target builds, bundled TinyCC validates the emitted translation unit before any
C file or executable is published. This validates C syntax and linkage, not V type semantics.
Unsupported V syntax and same-target TinyCC errors are reported directly; FastC never retries
through an AST-based backend.

FastC currently emits primitive functions and parameters, inferred local declarations, ordinary
expressions including comparison and logical operators, string interpolation for strings and
non-floating primitive values, `if`/`else`, and condition, C-style, infinite, and range `for` loops.
GNU `typeof` carries `:=` declarations into C. FastC infers representation metadata only when it is
needed to select a C spelling or runtime helper; it does not validate V type compatibility for
calls, returns, assignments, conditions, casts, operators, matches, ranges, or literal elements.
TinyCC may therefore accept source that the regular V checker rejects through C implicit
conversions. Use the regular backend when semantic type validation is required. Range bounds are
evaluated once, from left to right. The parser still rejects mutation of immutable or unknown local
names instead of relying on C's weaker assignment rules.

Syntax without a direct FastC lowering is rejected. In ordinary non-selfhost builds this includes
float printing, C-string and embedded-NUL string literals, runes, assertions, `sizeof`, shift,
division, modulo, indexing, parallel assignment, mixed-precedence expressions, oversized decimal
literals, and high-bit hexadecimal or binary literals. These restrictions avoid emitting C that
cannot provide the required runtime behavior.
FastC transports header paths, link inputs, frameworks, and preprocessor defines from `#flag`, and
resolves `#pkgconfig` options into its fixed TinyCC invocation. Other compile options remain
unsupported.

FastC requires exactly one `.v` entry file. Executables are host-target only; `-o file.c` also
permits an explicit cross target and publishes its generated C without host TinyCC validation.
Production, test, shared/live, ownership/autofree, object-file, profiling/coverage, strict C,
custom compiler, custom-builtin, `no_main`, `-Wimpure-v`, translated, and REPL modes are currently
rejected.

A conventional C-backend self-host prunes FastC along with the other optional backends. Pass
`-compile-backend fastc` or `-all-backends` when the generated compiler should retain `-b fastc`.

`-selfhost -b fastc -o v4 vlib/v3/v3.v` builds V3 using only the scanner-to-C path. The generated
compiler uses the small `v3.fastcdriver` entry point and can build further FastC generations without
the flat AST or conventional C backend. Set `V_MACOS_V3_NO_FALLBACK=1` while validating a chain to
turn any attempted compatibility fallback into a hard failure.

Set `FASTC_BENCH=1` when running a FastC self-host compiler to print the generation time and
`loc/s` for its input (`FASTC_BENCH_REPEAT=N` reports the best of N child runs).
`FASTC_BENCH_PHASES=1` prints the time of every generation phase, `FASTC_BENCH_FILES=1` the
generation time of every source file, and `FASTC_BENCH=1 FASTC_BENCH_LOOP=N` repeats generation N
times in-process so an external sampler can profile it. The compiler's own C preamble and runtime
(hashing, option boxing, tuple slots) are emitted by the generator that built it, so such changes
take effect one generation later: measure the compiler built by the modified compiler, not the
modified compiler itself.

Self-host generations box `?`/`!` payloads out of a per-thread bump chunk rather than `malloc`, keep
one file record per scanner pass, and honor `@[direct_array_access]` for string and array indexing.
Multi-return components larger than 32 bytes are boxed instead of copied into the tuple slot, so a
returned `map` or large struct can no longer overflow it.

Per-file generation, constant parsing, and the declaration and signature collection passes claim
their work items from a shared atomic counter (largest files first) instead of a static split, so
workers on faster cores take more files; the serial merge and output order is restored by index.
The first parallel pass also records which declaration keywords (`interface`, `$if`, type keywords,
generic `fn` syntax) each file mentions, and the later collection passes skip files that cannot
contain what they scan for.

Source resolution reads the program on worker threads before the ordering walk runs: each
imported module directory is listed on a thread and its files are read in chunks on further
threads, never more than the worker limit (CPU count or `VJOBS`) at once. The main thread joins
listings first and then the chunks in start order, resolving the imports of each chunk as it
lands, so reading one module overlaps with discovering the next. A resolve memo in `os.vtmp_dir()`
(`fastc_resolve_<hash>.memo`, keyed by the entry files, vroot, target and defines) records what
the previous resolution of the same entry touched, so the next run lists every directory, looks
up every module and stats every file in one batch instead of level by level along the import
chain; a file's content is taken from the memo's blob only when its size, mtime, ctime and inode
still match and it was last modified at least two seconds before the memo was written, and it is
read again otherwise. The memo also keeps each listed directory's file list and the entry
module's file list together with the directory's own stamp, so an unchanged directory is stat'ed
instead of listed again (adding, removing or renaming an entry changes that stamp, and the same
two-second rule applies); module lookups are recorded once per cache key, the memo's blob is read
in ranges by the same probe workers. The ordering walk itself is unchanged and replays over that
data, so the output is identical with and without the memo (`V3_FASTC_NO_RESOLVE_MEMO=1` disables
it). The type declarations are rendered on a worker while the signatures are collected, the
generic-method scan and the declaration index share one pass, while workers split oversized files
into generation fragments and build the by-name struct field index as the declaration phases
run, and the generated C is returned as ordered pieces (whole per-file bodies are shared rather
than copied into one buffer; only bodies cut around C directive lines are copied) that the drivers
write directly. Function bodies are pre-scanned for channel `select` statements only in files
whose bytes contain the word `select`. The declaration index records the text spans of each file's
constant and global declarations: a large file's constants are parsed as separate parallel
candidates (merged in source order), and the global phase parses only the recorded global
declarations instead of whole files.

A self-host build (`-selfhost`) for 64-bit macOS or glibc Linux emits C with no `#include`
at all: `gen/fastc/c_abi.v` holds, per target, the C library types, struct layouts, macros,
globals and function prototypes the emitted code uses, and that prelude replaces the header block
of the preamble; V's own C helper headers (`vlib/os/execute_capture_nix.h`, ...) are inlined
after it, and `#include` lines from V sources are left out. TinyCC then parses about 42K lines
instead of the 110K that the system headers expanded to. The build passes
`-Werror=implicit-function-declaration` (and no `-w`, which would silence it), so a C function
missing from the table fails the build instead of being called through an implicit `int`
prototype that truncates pointer results; `c_abi_test.v` compiles the table
against the host headers and checks every layout, size, value and prototype. The stitch also
drops the indexed functions (and enum `str`/print helpers) that nothing reachable from `main`,
the lifecycle hooks or the non-body pieces refers to, which the source-level name-grouped
reachability keeps: each worker records its definitions and the mangled names they mention, and
the assembly cuts the unreachable spans out of the pieces (about 9% of the self-host C).

The TinyCC step itself is split: `fastc_write_c_units` cuts the pieces into up to 8
translation units (the shared head of typedefs, prototypes and runtime in every unit, the
dispatch tables, lifecycle functions and `main` only in the first, the file bodies grouped by
size; the globals are definitions in the first unit and `extern` declarations in the others),
`fastc_compile_c_units` compiles them with concurrent `tcc -c` processes started through
`posix_spawn` (forking the large compiler process costs more), and one `tcc` call links the
objects (`VJOBS=1`, `V3_FASTC_NO_PARALLEL=1`, or `-no-parallel` keeps the single-file build; both
give the same C with `-keepc`). On macOS TinyCC runs Apple's `codesign` after linking, which costs
~50 ms per build: the drivers put a no-op `codesign` first on its PATH and ad-hoc sign the
executable themselves (`gen/fastc/macho_sign.v`, SHA-256 page hashes through CommonCrypto,
patched into the file in place). The SDK path is taken from `SDKROOT` or the toolchain selected by
`xcrun`; conventional SDK locations are used only as a fallback.

The standalone compiler supports `self` directly and defaults that command to FastC. For example,
`./v self x5` replaces the compiler through five descendant FastC generations, with each installed
generation compiling the next one. `-b fastc`, `-gc none`, `-cc tinyc|tcc`, `-keepc`, `-silent`,
and a single-generation `-o` destination are accepted.

Building the FastC self-host compiler with `-d arm64` selects its scanner-direct native path. That
path resolves sources with FastC's scanner passes, emits SSA while consuming parser tokens, and
passes the result to `v3.gen.arm64` for Mach-O output. It creates neither Flat AST nor C source and
does not run TinyCC. The native parser lives in `gen/fastc/arm64_d_arm64.v`; without the define,
that file and the ARM64/SSA imports are excluded and FastC retains its lightweight C path.

In selfhost mode, `t := spawn f(args)` and `t.wait()` lower to a generated pthread creator, run
wrapper, and join helper per spawned function: `thread` values are a typed wrapper around
`pthread_t` plus a heap block that packs the arguments and receives the result. Spawned threads
use an 8 MiB stack; `-thread-stack-size` is not yet supported by FastC. Thread allocation,
creation, and join failures are checked. Since V's `spawn` expression has no error return, these
runtime failures print a diagnostic and abort; packed arguments are released if thread creation
fails. Variadic, option/result, multi-return, and `mut`-argument callees, Windows targets, and
non-selfhost mode are rejected. Because bundled TinyCC has no thread-local storage, FastC compiler
generations build without the `prealloc` bump arena and use plain thread-safe `malloc`; the
FastC generation pipeline itself runs its per-file and reference-scan phases on spawned threads
in every generation.

The type system (`types/`) uses a `Type` sum type with 20 variants instead of
string-based type checks. Primitive types use a `Properties` flag enum with
`boolean`, `float`, `integer`, `unsigned` flags and a `size` field. The parser
produces string type names; `parse_type()` bridges them to structured `Type`
values. `resolve_type()` infers types from AST nodes, and `c_type()` lowers to C
type strings only at emission sites. Lexical scopes store `Type` values with
parent-chain lookups.

Sum types are compiled to tagged unions in C:
`struct Type { int typ; union { Variant1 _v1; ... }; };`.
Sum type construction (`Type(Variant{...})`), `is` checks, `as` casts, and
match-based smartcasting are all supported. The transformer lowers sum type
match branches to `is_expr` nodes, enabling smartcast field access through union
variants in both `if` and `match` blocks.

Type checking runs before transform, matching V1 and V2:
`TypeChecker.collect()` walks the flat AST to extract function signatures,
struct fields, enum names, type aliases, sum types, and C function declarations,
then registers runtime method signatures. `check_semantics()` validates the
unlowered source program before the transformer rewrites it. Both the C backend
and future backends receive the pre-populated `TypeChecker`.

After transform, v3 runs `annotate_types()`. This is not a second semantic
checker pass and should not report source diagnostics. It repopulates
expression-type metadata for the post-transform flat AST, including new node IDs
created by lowering. V1 and V2 do not need a separate step because their checker
updates the typed AST/table that later stages keep using directly; v3's flat AST
keeps those per-node caches outside the nodes.

Imports are resolved recursively: after parsing the input file, the driver
collects `import_decl` nodes, resolves module paths, parses module files, and
repeats until no new imports are found.

For C executable and library builds, v3 caches each imported module as a
declaration-only `.vh` file and a compiled `.o` file. A module object is rebuilt
when its source content, compiler implementation, target, or relevant build
configuration changes. `builtin`, `strconv`, `strings`, `hash`, `bits`, and
`math.bits` share one `builtin.o`, matching the v2 core-cache layout. Cache files live under
the V temporary directory by default; set `V3CACHE` to select another root, or pass
`-nocache`/`--no-cache` to disable the module cache. C-only `-o file.c` builds do not use the
object cache. An explicit `-b c` binary build also retains the complete generated translation unit
at `<output>.c` for codegen inspection. The benchmark output prints counts for parsed `.vh` and
`.v` files and their total
line counts immediately after the parse stage, followed by each category's space-separated paths
on one line. Paths below the current home directory use `~` as a prefix. A nonzero `.vh` count
shows how many cached module interfaces were parsed by that build. Required compile-time bodies
are embedded in the `.vh` interface, so a warm cached build parses `.v` files only from the input
program's directory; the `.v` list makes an unexpected module-cache miss visible. After
successfully populating module objects, a cold build prints a hint that unchanged modules will not
be recompiled on the next run.
Third-party C objects retain dependency manifests, so warm builds verify each unique source or
header once without launching a dependency-scanner process per object. Unchanged inputs use
nanosecond-resolution file metadata; a metadata change falls back to the recorded content hash.
This work is reported as the separate `C object cache` benchmark stage before `cc`.
On macOS, cached non-production implicit `run` builds combine the imported-module objects and
generated runtime prefix into a content-keyed dylib with the system C compiler. The remaining
current-directory program unit is compiled and linked against that dylib with bundled TinyCC.
Objective-C and framework compilation flags stay on the cached dylib side. This work is reported
as `C dylib cache`; the temporary executable retains an absolute runtime dependency on that cache
artifact and is removed after the run. An exact warm plan also restores its content-keyed TinyCC
executable and reports `cc (cached)`; project source, module object, C dependency, TinyCC input,
argument, or dylib changes invalidate it. Persistent outputs—including ordinary compilation,
explicit `run -o` output, and `-keepc` runs—are standalone. Production, shared-library, self-host,
explicit `-cc`, and `-nocache` builds also keep their existing direct-link behavior.
When the whole-program C plan is unchanged, v3 validates it immediately after parsing and reports
the check, mark-used, transform, type-annotation, monomorphization, and C generation stages as
cached. This avoids semantic and lowering work whose only consumer would be the cached C plan.
The pre-split main, TinyCC, and runtime-prefix sources are restored as `C module plan (cached)`.
Source, imported-module, native-input, compiler, target, flag, or configuration changes invalidate
the plan and run the complete diagnostic and generation pipeline normally.

## Architecture

```
source -> scanner -> fastc parser/C emitter -> TinyCC

source + vlib/builtin -> scanner -> flat parser -> flat AST -> imports
  -> check -> transform -> annotate types -> markused -> gen C -> cc
                                          \-> SSA build -> ARM64 gen -> link
                                          |            \-> optimize -> MIR -> insel (-prod)
                                          \-> gen WASM -> .wasm
```

The WebAssembly backend (`-b wasm`) walks the flat AST directly, like the C
backend, since WASM's structured control flow (`block`/`loop`/`if`/`br`) maps
cleanly from the tree and needs no relooping. It emits a self-contained `.wasm`
module via its own minimal binary encoder (LEB128 + section assembly, mirroring
how the ARM64 backend ships its own `asm`/`macho`/`linker`), so v3 stays
self-contained. The current scope is the integer/float core: functions with
numeric/bool params and locals, arithmetic, comparison, logical (short-circuit),
bitwise and shift operators, casts, `if`/`else`/`else if`, all `for` forms with
`break`/`continue`, direct calls, and recursion. `print`/`println` of string
literals, integers, and booleans is provided through WASI `fd_write` with a
built-in `itoa` helper. The module is a WASI command (`_start` calls `main`) and
also exports every compiled function for direct testing. Generics, strings as
values, structs, arrays, and maps are out of scope for now. Output runs under any
WASI runtime (e.g. `node:wasi`).

The parser directly emits a flat AST. There is no recursive AST intermediate and
no flatten step. All nodes live in a single `[]Node` array with children as
indices into a separate `[]NodeId` array. No pointer chasing, no recursive sum
types during code generation.

All `vlib/builtin/` files (38 files: both `.v` and `.c.v`) are parsed first to
collect struct, enum, type alias, interface, C function, and global definitions.
`$if` compile-time conditionals (`$if !no_bounds_checking`,
`$if gcboehm_opt ?`, `$if freestanding`, etc.) are resolved inline during
parsing. The parser evaluates the condition, parses only the taken branch, and
skips the other, so no `comptime_if` AST nodes reach the transformer or backends.

After parsing the input file, imports are resolved recursively: the driver scans
for `import_decl` nodes, resolves module paths, parses module `.v` and `.c.v`
files, and repeats until all transitive imports are loaded.

The type system (`types/`) uses a `Type` sum type with structured variants
instead of string-based type checks:
- **Primitive** types use a `Properties` flag enum and a `size` field. `int`,
  `i64`, `u8`, `f32`, and `bool` are all `Primitive` with different flags.
- **Compound** types: `Array{elem_type}`, `ArrayFixed{elem_type, len}`,
  `Map{key_type, value_type}`, `Pointer{base_type}`, `FnType{params,
  return_type}`, `OptionType`, `ResultType`, `MultiReturn`
- **Named** types: `Struct{name}`, `Enum{name, is_flag}`, `SumType{name}`, `Alias{name, base_type}`
- **Simple** tags: `Void`, `String`, `Char`, `Rune`, `ISize`, `USize`, `Nil`, `None`

`parse_type(string) Type` bridges parser string output to structured types.
`resolve_type(NodeId) Type` infers types from AST nodes. `c_type(Type) string`
lowers to C type strings only at final emission. Lexical scopes store
`map[string]Type` with parent-chain lookups.

`C.` structs and globals are recognized as extern C types and excluded from code
generation. Function bodies from builtins are skipped during C code generation;
only type and declaration information is used.

The transformer lowers match statements to if/else chains and collects
struct/global type info for its own type-dependent rewrites.

The markused pass performs reachability analysis from `main`, building a call
graph and BFS-walking to find all used functions. Method calls are resolved to
`Type.method` names using the type checker, reducing false positives from
syntactic matching. Both backends skip codegen for unreachable functions.

The ARM64 backend builds SSA IR from the flat AST, generates native ARM64
machine code, and links a Mach-O executable directly. The entire path from
source to binary uses no external tools.

## Code size

| Component      | Lines |
|----------------|-------|
| flat parser    | 3,129 |
| C gen (flat)   | 3,669 |
| type system    | 286   |
| type checker   | 974   |
| universe       | 97    |
| scopes         | 34    |
| SSA IR+build   | 1,510 |
| SSA optimize   | 474   |
| ARM64 gen      | 873   |
| ARM64 asm      | 634   |
| Mach-O         | 285   |
| ARM64 linker   | 1,478 |
| flat AST       | 231   |
| transformer    | 289   |
| markused       | 190   |
| driver         | 188   |
| pref           | 250   |
| scanner        | 593   |
| token          | 338   |
| bench          | 81    |
| **total**      | **~16,300** |

## Performance

Compiling `hello world` (`println('hello world')`) with full builtin import (38 files):

| Step      | Time     | RSS       |
|-----------|----------|-----------|
| parse     | 22 ms    | 10,880 KB |
| transform | 0.7 ms   | 11,024 KB |
| check     | 1.9 ms   | 11,664 KB |
| markused  | 2.2 ms   | 12,304 KB |
| gen C     | 1.5 ms   | 12,816 KB |
| write     | 0.2 ms   | 12,832 KB |
| cc        | 43 ms    | 12,864 KB |
| **total** | **~92 ms** | **12,864 KB** |

Compiling `test.v` (4,026 lines, 100 test sections):

Coverage includes structs, globals, match, recursion, nested loops, mut params,
assert, heap alloc, bitwise operations, pointers, nested structs, early return,
clamp, boolean chains, iterative algorithms, global counters, struct mutation,
fibonacci, vector math, matrix ops, prime checking, binary search, Ackermann,
triangle geometry, digital root, interpolation, bit manipulation, methods,
if-expressions, string interpolation, for-in range, enums, defer, unary ops,
array initialization, fixed-size arrays, println, algebraic optimizations, dead
store elimination, goto, optional unwrap, maps, dynamic arrays, array methods,
map iteration, strings.Builder, static methods, @FILE, unsafe blocks, and
function pointers.

**C backend:**

| Step      | Time     | RSS       |
|-----------|----------|-----------|
| parse     | 16 ms    | 11,456 KB |
| transform | 0.8 ms   | 11,872 KB |
| check     | 2.4 ms   | 12,528 KB |
| markused  | 127 ms   | 17,040 KB |
| gen C     | 10 ms    | 17,312 KB |
| write     | 0.1 ms   | 17,312 KB |
| cc        | 79 ms    | 17,312 KB |
| **total** | **~259 ms** | **17,312 KB** |

All v3 steps (parse + check + markused + transform + annotate types + gen +
write) complete in ~8 ms for hello world, including 38 builtin files, and
~157 ms for `test.v` with the C backend.

Peak RSS: 9-17 MB.

Compiling `v3.v` itself in the C self-host chain:

Commands:

```sh
v -prod -prealloc -d parallel -o v3 v3.v
./v3 -o v4 v3.v
./v4 -o v5 v3.v
./v5 -o v6 v3.v
```

v3 uses scoped bump arenas to release compiler-stage allocations explicitly. It refuses to build
with any Boehm GC mode or VGC and must be built with `-gc none`; `-prealloc` enables those arenas
and selects `-gc none` automatically. v3 also rejects collector modes and GC compile-time defines
for generated programs, which support `-gc none` only.

The standard v3 executable is built without `-d ownership`, so the ownership checker and its
analysis stages are compiled out. It rejects both `-ownership` and `-d ownership`; the main V
driver builds a separate ownership-enabled v3 executable only for an explicit `v -ownership`
invocation. Target compilations receive both `-ownership` and `-d ownership`, so the custom
`ownership` option is visible in target `$if` blocks and selects target `*_d_ownership.v` files.

The table uses the first v3-generated C stage, `./v3 -o v4 v3.v`. The plain
bootstrap includes thread support. v3 self-hosts parallel-capable successors by
default; pass `-no-parallel` or `--no-parallel` to disable threaded
transform/C codegen and omit parallel support from the self-hosted compiler
output. Debug builds use bundled TCC first, then fall back to `cc` only when
that compile fails.

Pass `-c99` to the v3 C backend to compile generated C and support objects as
C99 (`cc -std=c99`) instead of the default GNU11 mode. `test_all.vsh -c99`
validates the C backend and self-host chain in that mode, and skips the ARM64
native backend step because `-c99` only applies to generated C.

| Phase          | Time      | Peak RSS |
|----------------|----------:|---------:|
| parse          | 59.47 ms  | 73 MB    |
| check          | 46.57 ms  | 126 MB   |
| markused       | 39.81 ms  | 150 MB   |
| transform      | 70.79 ms  | 277 MB   |
| annotate types | 25.05 ms  | 309 MB   |
| gen C/write    | 53.75 ms  | 353 MB   |
| cc             | 746.53 ms | 353 MB   |
| **total**      | **1,042.14 ms** | **353 MB** |

## Comparison with V1

Frontend-only (parse + check + gen C, no `cc`):

| Compiler | hello world | test.v (3,756 lines) | Peak RSS (hello) | Peak RSS (test) |
|----------|------------|----------------------|------------------|-----------------|
| V1 (0.5.1) | 93 ms | 105 ms | 70 MB | 78 MB |
| **v3** | **8 ms** | **42 ms** | **9 MB** | **34 MB** |

v3 is **~3-12x faster** and uses **~3-8x less memory** than V1 for frontend compilation.

v3 parses all `vlib/builtin/` files (38 files: `.v` and `.c.v`) for type
definitions, C function declarations, and globals. `$if` compile-time
conditionals are resolved inline in the parser. Builtin function bodies are
skipped during C code generation; C runtime functions are provided via a compact
preamble.

Measured on macOS (Apple Silicon), warm runs. V1 built from `~/code/v5/v` (V 0.5.1).
