# v3

Clean rewrite of the V compiler. Reuses v2's scanner, uses a flat AST parser
with Pratt parsing, a structured type system with sum-type variants, lexical
scoping, a transformer for AST simplification, a shared type-checking phase, a
markused pass for dead-code elimination, recursive import resolution, and three
backends: a direct flat-AST-to-C backend, a native ARM64 backend via SSA IR with
a built-in linker, and a direct flat-AST-to-WebAssembly backend. With `-prod`,
the ARM64 backend runs SSA optimization, MIR lowering, and instruction
selection.

Imports all `vlib/builtin/` V source files, both pure V (`.v`) and C-interop
(`.c.v`), for struct, enum, type alias, interface, C function declarations, and
global definitions. `$if` compile-time conditionals are resolved directly in the
parser. The parser evaluates the condition, parses only the taken branch, and
skips the other, so no AST nodes or transformer pass is needed for `$if` blocks.
`#include` and `#flag` directives inside `$if` blocks are handled correctly: the
scanner consumes the entire directive line as a single token, preventing the
parser from reading past block boundaries. File selection filters out
arch-specific files (`.arm64.v`, `.amd64.v`) and deduplicates function
definitions when both `.v` and `.c.v` files exist. C runtime functions
(println, string ops, int_str, etc.) are still provided via a built-in preamble;
builtin function bodies are skipped during C code generation. Maps use the
builtin `map` type name and API (`new_map`, `map__set`, `map__get`,
`map__delete`, etc.) with a simplified open-addressing implementation until v3
can compile the full builtin map.v.

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
the V temporary directory by default; set `V3CACHE` to select another root, or
pass `-nocache`/`--no-cache` to disable the module cache. C-only `-o file.c`
builds do not use the object cache.

## Architecture

```
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
v -gc none -prod -o v3 v3.v
./v3 -o v4 v3.v
./v4 -o v5 v3.v
./v5 -o v6 v3.v
```

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
