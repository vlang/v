<!-- V_AGENTS.md  v1.1  2026-02-04 -->

# V Repo Guide

Practical quick reference for the V compiler, standard library, and tools.
Written for AI coding agents; useful for humans too.

## Contents
* Quick Start
* Top Rules
* Agent Rules
* Safety (Do Not Brick the Repo)
* Divergences From Repo Docs
* Quick Decisions
* Common Workflow
* Reporting
* Prerequisites
* Build & Rebuild
* Run Programs
* Testing
* Code Style
* Modules and Imports
* C/JS Interop Hygiene
* Environment-Specific Code (files and `$if`)
* Compile-Time Code and Reflection
* Debug
* Compiler Architecture
* Key Directories
* Test Locations
* Error Reporting (checker/parser)
* Option/Result Types
* Tools
* Commits and PRs
* Environment Variables
* Gotchas

## Quick Start
Get operational from the repo root in three steps:

1. Build once (only if `./v` is missing): `make`
2. Build a working compiler:
   * Debug-friendly (recommended): `./v -g -keepc -o ./vnew cmd/v`
3. Use `./vnew` for everything:
   * Run a file: `./vnew run examples/hello_world.v`
   * Run tests: `./vnew -silent test vlib/v/`
   * Format: `./vnew fmt -w path/to/file.v`

Then read Top Rules and Agent Rules before making changes.

## Top Rules
* Use `./v` only to build `./vnew`; use `./vnew` for everything else.
* Put all V flags immediately after `./vnew` and before the
  subcommand/file, e.g. `./vnew -g run file.v`
  (not `./vnew run file.v -g`); flags after the subcommand are passed
  to that subcommand.
* Rebuild `./vnew` after compiler or core module changes
  (see Build & Rebuild).
* Run the smallest relevant tests; see Testing for triggers and
  minimums.
* Ask before large refactors or wide file touches (see Agent Rules).
* Do not stash or modify unrelated files unless explicitly instructed.
* This guide assumes agents run locally, not in CI; CI notes are
  informational only.
* When a summary is required, include behavior change, tests run, and
  touched file paths.
* If instructions overlap, prefer Build & Rebuild, Testing, and
  Reporting.
* If duplicates drift, treat Build & Rebuild, Testing, and Reporting as
  canonical and align other sections to them.

## Agent Rules

### Repo root
All commands assume the repo root as the working directory. The default
location is `/opt/v`, but this may differ in your environment. If a
command fails due to missing paths, verify with `pwd` and adjust
accordingly. Use a per-command workdir only when a task requires a
subdir.

### Tone and output
* Be concise by default. If the user asks for depth, provide it while
  keeping structure tight.
* Keep output easy to scan: short sections, bullets when listing,
  commands in backticks, no filler.
* Use a strict, operational tone unless higher-priority instructions
  override it.
* Ask only when required. If information is missing, ask a direct
  question.

### File access and edit scope
* You may read and edit all files in the V repo without asking for
  permission. This is file access, not change scope; scope is
  constrained below. Reading is always OK.
  Edits to `ci/` or `Dockerfile*` still require an explicit ask.
* Run build, test, and format commands without asking for permission.
  These are validation steps, not code changes. Only ask about edit
  scope, not about running `fmt`, targeted tests, or `check-md`.
* Only modify files required for the user request; avoid unrelated
  refactors. If duplication is harmful, small refactors to remove it
  are OK only when needed. Only refactor duplication in code you are
  already touching, and only when it directly supports the request or
  fixes a bug there. "Touching" means files already modified for the
  request.
* Avoid unrelated file changes; call them out if present.
* Avoid touching `thirdparty/` unless explicitly requested. If changes
  are needed there, ask for approval before proceeding.

### When to ask
* Ask before large refactors or wide file touches (more than 5 files,
  or changes across multiple repo-root directories like `cmd/`, `vlib/`,
  `doc/`, `examples/`).
  Exception: docs-only changes across many files are OK without asking;
  call them out in the summary.
* Ask before large behavioral changes within a single subsystem or file,
  even if the file count is small. Examples: changes to parser rules,
  checker resolution, codegen output shape, diagnostic text/ordering, or
  tool CLI behavior. Large means user-visible changes in CLI flags,
  output, diagnostics, or codegen shape. If unsure whether a change is
  "large," ask.
* Ask before touching `ci/` or `Dockerfile*` unless explicitly
  requested. If changes are needed there, confirm whether local
  validation is expected or if CI-only coverage is acceptable.

### Bootstrap
* Bootstrap/compiler usage rules: see Top Rules and Build & Rebuild.

### Completeness and summaries
* If you cannot complete a requested step, state the blocker and partial
  progress (what was attempted and what remains).
* After substantial work, provide a short summary and list touched file
  paths. Substantial work means any behavioral change, or changes in
  more than one file. Always include tests run (or "Not run" with
  reason) in the summary.
* Never change tests just to silence failures. Update expectations only
  when behavior changes are intended, and note the rationale in the
  summary.

### New files and deeper guidance
* New file checklist: format with `./vnew fmt -w`, add doc comments for
  any public functions, run `./vnew check-md` for markdown files, and
  keep Markdown lines <= 100 chars. Add or update tests when introducing
  a new public API.
* For deeper edge cases, consult `CONTRIBUTING.md` and `TESTS.md`.

## Safety (Do Not Brick the Repo)
* Do not overwrite the working `./v` binary.
  * Never run `./v self` without `-o`.
  * Build with `./v -o ./vnew cmd/v`, then use `./vnew` for all checks.
* Keep generated C when debugging backend issues: add `-keepc`.
* Avoid hidden/bidirectional Unicode characters in source/markdown
  files.
* If the compiler becomes unusable, recover with:
  1. `git stash`
  2. `make`
  3. `git stash apply`
  Check `git status` before stashing to avoid hiding unrelated work.
  Do not stash unless explicitly instructed or the compiler is bricked.

## Divergences From Repo Docs
The repo docs use `v` in examples. In this environment:
* Use `./v -g -keepc -o ./vnew cmd/v` instead of `v self`.
* Use `./vnew` for all builds, runs, and tests.
* `TESTS.md` suggests `v test-all` before PRs; ask before running
  `./vnew test-all`.
These overrides exist to keep agent workflows reproducible and to avoid
breaking the bootstrap compiler.

## Quick Decisions
* For rebuild and test choices, follow Build & Rebuild and Testing.
* Follow Common Workflow for the default execution order.
* For broader workflow guidance, see `CONTRIBUTING.md`.

### Compact decision table (rebuild/tests)
Use this table to pick the minimum rebuild/tests quickly. See Build &
Rebuild and Testing for full details and edge cases.
Commands omit `./vnew` for brevity; assume the `./vnew` prefix.
This table is the minimum set; check Testing for additional triggers.
REPL and backend changes have additional triggers in Testing.
Note: `cmd/v/` is compiler scope; treat changes there as compiler
changes.
When in doubt, ask before proceeding.
If you read only one section for tests, **read Testing**.

| Change area | Rebuild? | Minimum tests to run |
| --- | --- | --- |
| Docs only (`.md`) | No | `check-md file.md` |
| Compiler (`vlib/v/`, `cmd/v/`) | Yes | `-silent vlib/v/compiler_errors_test.v`; `test vlib/v/` |
| Core modules (builtin/strings/os/strconv/time) | Yes | Smallest relevant tests |
| Parser-only (`vlib/v/parser/`) | Yes | `test vlib/v/parser/` |
| Checker-only (`vlib/v/checker/`) | Yes | `test vlib/v/checker/` |
| Comptime (`vlib/v/comptime/`) | Yes | `test vlib/v/tests/`; comptime-related tests |
| vlib (non-compiler) | No | Nearest `*_test.v` or `test vlib/path/` |
| Tools (`cmd/tools/`) | No | Tool-specific test; else nearest `*_test.v` |
| Diagnostic/output changes | Yes | `vlib/v/slow_tests/inout/compiler_test.v` |
| C codegen (`vlib/v/gen/c/`) | Yes | `vlib/v/gen/c/coutput_test.v` |

## Common Workflow
0. Before work: `git status`; ensure `./vnew` exists; rebuild if needed.
   If `./vnew` is missing, see Quick Start or Build & Rebuild.
1. Edit the relevant files.
2. If compiler sources or core modules changed, rebuild `./vnew` with
   `./v -g -keepc -o ./vnew cmd/v` (see Build & Rebuild).
3. Format touched `.v`/`.vsh` files and run `./vnew check-md` on
   touched markdown.
4. Run the smallest relevant tests for the change scope (see Testing).

See Build & Rebuild for rebuild triggers and flags.

## Reporting
* When a summary is required (see Agent Rules), it must include:
  * Behavior changes (or "No behavior change").
  * Tests run (or "Not run" with a reason).
  * Touched file paths.
  * Note unrelated changes if present.
  * Note doc updates if public behavior/tool output changed.
* If public behavior or tool output changes, update relevant docs
  (README.md, `doc/`, `tutorials/`) and note it.
  For example: README.md for top-level CLI usage, `doc/` for
  compiler/tool docs, `tutorials/` for learning material.
* If you add or change a public API, update module docs or README and
  note it. Public API includes stdlib functions/types and user-visible
  compiler flags, diagnostics, tool CLI behavior (including `cmd/tools`
  and stdlib CLI tools), or output formats.
* Update `CHANGELOG.md` or `ROADMAP.md` only when explicitly requested.
* Public behavior includes compiler output, diagnostics, user-facing
  CLI, and stdlib API. This includes developer-facing flags, error
  codes, or output ordering changes.
* Internal refactors with no public behavior change do not require doc
  updates.
* If unsure whether behavior is public, ask the user before updating
  docs.
* Acceptable reasons for not running tests: docs-only change, no
  relevant tests, or environment constraints. Be specific.
* Docs-only changes: run `./vnew check-md file.md`; no other tests
  required unless a test explicitly reads those docs.
  No rebuild is needed unless compiler or core modules changed.
* For docs-only, explicitly mention `check-md` in the summary.
* If you update `.out` files, state the rationale in the summary.
* Do not update `.out` files unless a behavior change is intended;
  otherwise treat mismatches as regressions.
* Example summary line: `Behavior change: none` or
  `Behavior change: fixed X`.
* Behavior change includes output, API, error messages, and test
  expectations.
* If blocked, note what was attempted and why it failed.
* If tooling/network restrictions block a step, state the restriction
  and the closest viable alternative.
* Use `git status` to confirm touched files before reporting.

## Prerequisites
* Toolchain: `make`, `git`, and a C compiler (`clang` or `gcc`).
* Windows: use `make.bat` for the initial build.

## Build & Rebuild
* Initial build (only if `./v` is missing): `make`
  (Windows: `make.bat`).
* Build `./vnew` (debug-friendly, recommended for agent workflows):
  `./v -g -keepc -o ./vnew cmd/v`
* Never run `./v self` directly; only build `./vnew` with the commands
  above.
* If `./v` is missing, run `make` first, then build `./vnew`.
* If `./vnew` is missing but `./v` exists, run `./v -o ./vnew cmd/v`.
* This section is the source of truth for rebuild triggers.
  If a rule appears elsewhere, defer to this section.
* Rebuild triggers:
  * Compiler sources in `vlib/v/` or `cmd/v/`.
  * Core modules: `builtin`, `strings`, `os`, `strconv`, `time`.
* If `./v` exists but compiler sources changed, still rebuild `./vnew`
  before tests.
* If unsure whether compiler/core changes happened, rebuild `./vnew`.
* Common flags:
  * `-g` debug info (V line numbers).
  * `-cg` debug info (C line numbers); often combined with `-keepc`.
  * `-keepc` keep generated C file(s).
  * `-prod` optimized build.
  * `-o file` output path/name.
  * `-cc clang` pick a C compiler.
  * `-cstrict` be stricter about the generated C.
  * `-b js|native|wasm` select a backend.
  * `-os <os>` cross-compile target selection (when supported).

## Code Style
* Comments: add succinct comments only when code is not self-explanatory.
  Do not delete existing comments unless they are incorrect; you may fix
  grammar or spelling without changing meaning.
  Add V doc comments right before each new or modified public function
  or method.
  The V doc comments should start with the name of the fn,
  example: `// the_name does ...`
* Copy pasta: avoid copy pasta. If there's duplicate logic, move to a
  function only when it is required for the request and within code you
  are already touching.
* Avoid using `unsafe{ code }` blocks where possible, and minimize
  their scope. `unsafe` is acceptable for low-level interop (e.g. C
  pointer casts, manual memory management) but should never wrap
  ordinary V logic. When `unsafe` is required, keep the block as small
  as possible and add a comment explaining why it is necessary.
* Keep Markdown lines <= 100 chars (the checker is strict).
  Apply to touched lines in modified Markdown files.
* Avoid hidden/bidirectional Unicode characters in source/markdown
  files.
* Non-V files: keep existing formatting; only reformat if required by
  the change.
* Treat `.vv` files in `vlib/v/slow_tests/inout/` as fixtures; avoid
  formatting unless a behavior change is intended and output
  expectations are updated.
* Formatting and check commands are in Tools.

## Modules and Imports
* Module names must match their directory name (no hierarchy in the
  `module` line). Mismatches cause silent import failures.
* Imports follow the folder hierarchy (e.g. `import abc.def`).

## C/JS Interop Hygiene
* Avoid using `C.` or `JS.` symbols in plain `.v` files.
  * Use `.c.v` / `.js.v` files.
  * Enable `-Wimpure-v` to catch accidental impurity.
* Do not refactor existing `C.`/`JS.` uses in `.v` files unless
  required. If you must change those lines, prefer moving the interop
  code to `.c.v`/`.js.v`.
* For C interop, use correct `const_` prefixes in C function
  redeclarations when needed (helps with `-cstrict` and C static
  analysis tooling).

## Environment-Specific Code (files and `$if`)
V supports environment-specific file suffixes. Prefer them when the
whole file is platform/backend specific.

Common patterns:
* Backend splits:
  * `*.c.v` (C backend), `*.js.v` (JS backend), `*.native.v` (native
    backend), `*.wasm.v` (WASM backend)
* OS splits:
  * `*_windows.c.v`, `*_linux.c.v`, `*_nix.c.v`, with `*_default.c.v`
    as fallback
* Flag splits:
  * `*_d_customflag.v` is included only with `-d customflag`
  * `*_notd_customflag.v` is included only when that flag is NOT passed

Notes:
* Do not use combinatorial suffixes like `_d_flag_linux.c.v`; use
  `_d_flag.v` plus `$if linux {}` inside the file.
* For smaller platform differences, use compile-time `$if` blocks:
  * `$if windows { ... } $else { ... }`
* See Compile-Time Code and Reflection for the full `$if` reference.

## Compile-Time Code and Reflection
V uses `$` as a prefix for compile-time (comptime) operations. These
are evaluated by the compiler, not at runtime. AI agents frequently
confuse comptime and runtime constructs; this section clarifies the
boundaries.

### Compile-time `$if`
`$if` evaluates conditions at compile time. It is not a runtime `if`.
Use it for platform, compiler, and custom-flag checks:
* `$if windows { ... } $else $if linux { ... } $else { ... }`
* `$if debug { ... }` (enabled by `-g` or `-cg`)
* `$if prod { ... }` (enabled by `-prod`)
* `$if custom_flag ? { ... }` (enabled by `-d custom_flag`)

Full list of builtin `$if` options: see the table at
<https://docs.vlang.io/conditional-compilation.html>.

Common mistakes:
* Using runtime `if` where `$if` is needed for platform-specific code.
  Runtime `if` will fail to compile if it references platform-specific
  symbols; `$if` excludes the block entirely on non-matching platforms.
* Forgetting the `?` suffix for custom flags: `$if myflag ? { ... }`.
  Without `?`, the compiler treats it as a builtin option and silently
  ignores it.

### Compile-time `$for`
`$for` iterates over type metadata at compile time. It works with:
* `StructType.fields` - iterate struct fields
* `StructType.methods` - iterate struct methods
* `EnumType.values` - iterate enum values
* `StructType.attributes` - iterate struct attributes
* `SumType.variants` - iterate sum type variants
* `method.params` - iterate method parameters

Inside `$for` blocks, use `$if` to branch on field/method types:
```v oksyntax
fn main() {
	$for field in MyStruct.fields {
		$if field.typ is string {
			println(field.name)
		}
	}
}
```

Also works with generics: `T.fields`, `param.fields` where
`fn gen[T](param T) {}`.

Common mistakes:
* Using runtime `for` to iterate struct fields. This does not work;
  V has no runtime reflection. Always use `$for`.
* Calling `obj.$method()` outside a `$for m in Type.methods` block.
  The `$method()` call is only valid inside a comptime method
  iteration.
* Assuming comptime `$for` produces a runtime loop. It does not; the
  compiler unrolls it into concrete code for each field/method/variant.

### Compile-time functions
Only the following `$`-prefixed functions are supported:
* `$embed_file('path')` - embed a file's contents into the binary.
  Paths can be absolute, relative to the source file, or use pseudo
  variables like `@VEXEROOT` or `@VMODROOT`.
* `$tmpl('path')` - compile a V template file (used by vweb/veb).
* `$env('VAR')` - read an environment variable at compile time.
* `$d('ident', default)` - read a `-d ident=value` compile-time
  define, with a default fallback.
* `$res('path')` - embed a resource (Android).
* `$compile_error('msg')` - emit a compile-time error.
* `$compile_warn('msg')` - emit a compile-time warning.
* `$pkgconfig('name')` - query pkg-config at compile time.

Common mistakes:
* Inventing comptime functions that do not exist (e.g. `$typeof`,
  `$sizeof` as comptime calls). Use `typeof(expr).name` and
  `sizeof(Type)` instead; these are builtins, not `$`-prefixed.
* Assuming `$embed_file` returns a string. It returns an
  `EmbedFileData` object; use `.to_string()` or `.to_bytes()`.
* Using `$env` where a runtime `os.getenv` is appropriate, or vice
  versa. `$env` is baked in at compile time and cannot change at
  runtime.

### Compile-time pseudo variables
These are `@`-prefixed identifiers substituted at compile time:
* `@FN` - current function name.
* `@METHOD` - `ReceiverType.MethodName`.
* `@MOD` - current module name.
* `@STRUCT` - current struct name.
* `@FILE`, `@DIR`, `@LINE`, `@COLUMN`, `@FILE_LINE` - source
  location.
* `@LOCATION` - file, line, and current type+method; good for logging.
* `@VEXE`, `@VEXEROOT` - path to the V compiler and its directory.
* `@VHASH`, `@VCURRENTHASH` - compiler commit hashes.
* `@VMOD_FILE`, `@VMODHASH`, `@VMODROOT` - nearest `v.mod` info.
* `@BUILD_DATE`, `@BUILD_TIME`, `@BUILD_TIMESTAMP` - build time (UTC).
  Override with the `SOURCE_DATE_EPOCH` env var for reproducible
  builds.
* `@OS`, `@CCOMPILER`, `@BACKEND`, `@PLATFORM` - build environment.

### Compile-time type groups
Comptime type groups combine multiple types into a higher-level type
for use in generic or comptime `$if` checks:
* `$int` - all integer types
* `$float` - all float types
* `$array` - all array types
* `$map` - all map types
* `$struct` - all struct types
* `$enum` - all enum types
* `$alias` - all type aliases
* `$sumtype` - all sum types
* `$function` - all function types
* `$interface` - all interface types
* `$option` - all option types

Example: `$if field.typ is $int { ... }`

### Comptime changes and testing
Comptime logic lives in `vlib/v/comptime/` and is exercised by the
checker, parser, and cgen stages. Changes here require a rebuild of
`./vnew` and should be tested with `./vnew -silent test vlib/v/tests/` plus
any comptime-specific tests. See the decision table and Testing.

## Run Programs
* Compile and run: `./vnew run file.v`.
* Just compile: `./vnew file.v` (creates executable).
* With debug info: `./vnew -g run file.v`.
* Debug run (C line numbers): `./vnew -keepc -cg run file.v`.
* Example: `./vnew run examples/hello_world.v`.

## Testing
Run:
* File (shows test output): `./vnew path/to/file_test.v`.
* File (test runner report only): `./vnew test path/to/file_test.v`.
* Dir: `./vnew -silent test path/to/dir/`.
* Dir with statistics/metrics: `./vnew -stats test path/to/dir/`.
* Compiler: `./vnew -silent vlib/v/compiler_errors_test.v`.
* Fix outputs (only when intended):
  `VAUTOFIX=1 ./vnew -silent vlib/v/compiler_errors_test.v`.
* All: `./vnew test-all`.
  Ask before running `./vnew test-all` unless explicitly requested.

When:
* Rule of thumb: for localized changes, run the smallest relevant
  tests. If any trigger below matches, run the listed tests.
* Minimum tests listed are the floor, not the ceiling; add targeted
  tests for cross-cutting changes.
* If unsure which tests apply, ask the user before proceeding.
* If in doubt, prefer the smallest targeted test and ask.
* Run all tests that apply. Start with the smallest targeted tests;
  add slow tests as needed. Order does not matter.
* Compiler changes (`vlib/v/` or `cmd/v/`):
  Run `./vnew -silent vlib/v/compiler_errors_test.v`,
  `./vnew -silent test vlib/v/`.
* Parser-only changes (`vlib/v/parser/`):
  Run `./vnew -silent test vlib/v/parser/`.
* Checker-only changes (`vlib/v/checker`):
  Run `./vnew -silent test vlib/v/checker/`.
* vlib changes: Run nearest `*_test.v` or
  `./vnew -silent test vlib/path/`.
* Tool changes (`cmd/tools/`): Run tool-specific tests. If none exist,
  run the smallest relevant `*_test.v` that exercises the tool.
  Note: `cmd/v/` is compiler scope, not tools.
  Examples: `cmd/tools/vfmt` -> `vlib/v/fmt/fmt_test.v` .
  `cmd/tools/vdoc` -> `cmd/tools/vdoc/vdoc_test.v`.
* Diagnostic/output changes:
  Run `./vnew -silent vlib/v/slow_tests/inout/compiler_test.v`.
* C codegen changes: Run `./vnew -silent vlib/v/gen/c/coutput_test.v`.
  Consider a stricter validation pass:
  `./vnew -cstrict -cc clang -silent test vlib/v/`.
* REPL changes: Run `./vnew -silent vlib/v/slow_tests/repl/repl_test.v`.
* Broad refactors: Run `./vnew -silent test-all`.
* Backend-specific changes: run the smallest relevant tests for the
  affected backend. JS/native/WASM backends are incomplete, so avoid
  broad `-b <backend> test vlib/` runs. Prefer targeted `*_test.v`
  files or small test dirs with `-b js|native|wasm`.

If time-constrained, prioritize
`./vnew -silent vlib/v/compiler_errors_test.v` and the smallest targeted tests.
Run `vlib/v/slow_tests/inout/compiler_test.v`
and `vlib/v/gen/c/coutput_test.v` when output or codegen changes are
likely.
See `TESTS.md` for more guidance on test selection and output
expectations.
See `CONTRIBUTING.md` for broader workflow guidance.

Concrete triggers:
* `vlib/v/slow_tests/inout/compiler_test.v` when error text or output
  formatting changes, or changes in checker/parser error reporting.
* `vlib/v/gen/c/coutput_test.v` for changes under `vlib/v/gen/c/` or
  C codegen output paths.

Types:
* Standard: `*_test.v` files with `test_` functions.
* Output: `.vv` source + `.out` expected output in
  `vlib/v/slow_tests/inout/`.
  Example: `./vnew -silent vlib/v/slow_tests/inout/compiler_test.v`.
* `vlib/v/tests/**` may use `.run.out` expectations; run with
  `./vnew -silent test vlib/v/tests`.

Docs-only guidance: see Reporting.
If time-boxed, run at least the smallest relevant test and note
skipped coverage in the summary.

### Useful env variables and flags while testing
* `VAUTOFIX=1` - Auto-update .out files when tests fail (run twice).
  Use only when a behavior change is intended.
* `VTEST_ONLY=glob_pattern` - Run only tests matching pattern.
* `VTEST_HIDE_OK=1` - Hide successful tests, show only failures.
* `./vnew -silent test path/to/dir/` - Show only failed tests (if any), and a summary report.
* `-cc tcc` can speed test builds when TCC is available.
* Output expectations: update `.out` files only when behavior changes
  are intended; note the rationale in the summary.

## Debug
* See what the C compiler is doing:
  * `-showcc` prints the C compile command.
  * `-show-c-output` prints the C compiler output.
* Build a temporary compiler for debug flags:
  * `./vnew -o ./w -d trace_checker cmd/v`
  * Then run: `./w file.v`
* Keep and inspect generated C:
  * `-keepc -cg` is the common combo.
  * `-printfn <name> -o file.c` emits only the named C function to standart
    output. The `name` uses the `modulename__fnname` format (e.g.
    `main__main`). This flag can be repeated to print multiple
    functions. Methods/generics may use more complex C names; use
    `-keepc` to confirm exact symbols.
* Trace stages:
  `-d trace_scanner|trace_parser|trace_checker|trace_gen`.
  These flags can help diagnose a problem when a stage stops earlier
  than expected.
* Time stages: `-d time_parsing|time_checking`.
* V panics: `-keepc -g`.
* C segfaults: `-keepc -cg -cc clang`.
* JS/native/wasm: prefer small, focused test files with
  `-b js|native|wasm` and `-show-c-output` where applicable; avoid
  broad `-b <backend> test vlib/`.
* Quick code location technique:
  When debugging code generation issues, add unique comment tags to
  relevant code generation points (e.g., `/*tom51*/`) in the compiler
  source. Rebuild and generate C code, then search the generated file for these
  tags to quickly map generated C code back to the exact compiler source
  location. This is especially useful when multiple code paths generate
  similar-looking output and you need to identify which path is actually used.
  Example:
  ```v
  // In vlib/v/gen/c/assign.v:
  g.write('builtin___option_ok/*tom51*/(&(${styp}[]) { ')

  // After rebuild, search generated C code for "tom51":
  // builtin___option_ok/*tom51*/(&(int[]) { ... });
  ```
  Remember to remove these debug tags after fixing the issue.

## Compiler Architecture
The V compiler has the following stages, orchestrated by the
`v.builder` module:
`v.scanner` -> `v.parser` -> `v.checker` -> `v.transformer` ->
`v.markused` -> `v.gen.c`
Their corresponding folders are: vlib/v/scanner, vlib/v/parser,
vlib/v/checker, vlib/v/transformer, vlib/v/markused, vlib/v/gen/c .
There are additional subsystems (supporting or optional compiler
modules) like v.comptime, v.generics, v.pref, v.reflection,
v.callgraph, etc.

### Key Directories
* `vlib/`: Standard library (changes here can affect the compiler
  itself).
* `cmd/v/v.v`: Compiler entry.
* `cmd/tools/`: vfmt, vdoc, vup, vquest, etc.
* `examples/`: Example programs.
* `thirdparty/`: Bundled C libraries (tcc, mbedtls, sokol, etc.).
* `vlib/v/`: Compiler modules.
  * ast/ - AST node definitions
  * fmt/ - Code formatter
  * scanner/ - Tokenizer
  * token/ - Token definitions
  * parser/ - Produces AST from tokens
  * checker/ - Type checking and resolution
  * comptime/ - Compile-time evaluation support
  * transformer/ - Common optimisations and simplifications, makes the
    backends simpler
  * markused/ - Dead code eliminator
  * gen/c/ - C code generation (primary backend, known as cgen)
  * gen/js/ - JavaScript backend
  * gen/native/ - Machine code generation (ELF, Mach-O)
  * gen/wasm/ - WebAssembly backend

### Test Locations
* `vlib/v/tests/`: Compiler feature tests (including comptime).
* `vlib/v/slow_tests/`: Output-matching and slow tests for the
  compiler.
* `vlib/v/slow_tests/inout/` - Output comparison tests
  (.vv + .out pairs)
* `vlib/v/parser/` - Parser error tests
* `vlib/v/checker/` - Checker error tests
* `vlib/v/gen/c/testdata/` - C codegen tests (.vv + .c.must_have)
* `examples/compiletime/` - Comptime usage examples (including
  `reflection.v`)

## Error Reporting (checker/parser)
* Error: `c.error('message', pos)` - hard error, stops compilation.
* Warning: `c.warn('message', pos)` - warning, allows compilation.
* Notice: `c.note('message', pos)` - informational only.
* Pattern: Most checker methods use `fn (mut c Checker)` receiver.
* Location: `vlib/v/checker/errors.v`.

## Option/Result Types
* Syntax: `?Type` (optional, can be none) vs `!Type` (result, can
  error).
* Common bugs: Unwrapping in if guards, struct init with option fields,
  ternaries with options.
* When changing option/result behavior, add a regression test in
  `vlib/v/tests/`.
* Tests: Search `vlib/v/tests/` for option/result test files.
* Pitfall: Options in ternaries, SQL statements, and fixed arrays need
  special handling in cgen.

## Tools
* Note: if a rule overlaps with Testing, follow Testing.
* Format: `./vnew fmt -w <file>` for touched `.v` and `.vsh` files.
  Format only touched files unless explicitly asked to reformat broader
  scope.
* Treat new files as touched for formatting and markdown checks.
* Check *all* files are formatted: `./vnew -silent test-fmt`.
  Run only when asked or when validating the full tree.
* Check markdown: `./vnew check-md file.md` for touched `.md` files
  (required before commits).
* Code style checker: `./vnew vet vlib/v`
  Run only when asked or when making broad checker changes (more than
  3 files in `vlib/v/checker/`).
* Module docs: `./vnew doc -readme -all -l module_name`.
* Search: `rg pattern` (or `git grep`); list files: `rg --files`.
* Auto-format hook: `./vnew git-fmt-hook install`.

## Commits and PRs
* See `CONTRIBUTING.md` for full commit message conventions and PR
  workflow.
* Keep commits focused: one logical change per commit.
* If your branch has diverged and a rebase or merge is needed, do not
  force-push or drop hunks without asking. State the conflict and let
  the user decide the resolution strategy. When in doubt, prefer
  `git rebase` over `git merge` for a linear history, but always ask
  if the situation is ambiguous.

## Environment Variables
* VFLAGS: Pass flags to all V invocations
  (e.g., `VFLAGS='-g' ./vnew test-all`).
* VEXE: Path to V compiler executable (useful in CI/scripts).
* TMPDIR: Controls where `.tmp.c` files are written (V uses
  `TMPDIR/v/`).
* SOURCE_DATE_EPOCH: Override build timestamp for reproducible builds
  (affects `@BUILD_DATE`, `@BUILD_TIME`, `@BUILD_TIMESTAMP`).
* V2-specific: `V2CC`, `V2CFLAGS`, `V2VERBOSE` (for v2 development).

## Gotchas
* Core modules (`builtin`, `strings`, `os`, `strconv`, `time`) can
  affect the compiler because it is a V program. Rebuild `./vnew` when
  they change.
* Module names must match their directory name; mismatches cause silent
  import failures. See Modules and Imports.
* Some V programs and tools hardcode `os.execute('v ...')` in their
  source. The `./vnew` workflow does not protect against these; if you
  encounter unexpected behavior from such calls, check whether the
  code is invoking the system `v` instead of `./vnew` and adjust
  accordingly.
* Stale `./vnew` can cause confusing failures; rebuild if behavior
  seems off.
* Output tests require exact matches; whitespace changes break tests.
* Formatting reminders: follow Code Style for `fmt` and `check-md`.
* C compilation errors? Check generated C with `-keepc`
  (creates `/tmp/*.tmp.c`; see also TMPDIR in Environment Variables).
* Compile-time code (`$if`, `$for`, `$embed_file`, etc.) is a common
  source of AI mistakes. See Compile-Time Code and Reflection for
  correct usage and pitfalls.
* Broken compiler? See Safety for the recovery sequence.
* Consult `CONTRIBUTING.md` and `TESTS.md` for:
  * New tests or test infra changes.
  * Output tests or `.out` updates.
  * Broad refactors or compiler-wide changes.
  * Unsure which slow test runner applies to your change.
