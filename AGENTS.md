# V Repo Guide

Quick reference for the V compiler, standard library, and tools.

## Contents
* Agent Rules
* Common Workflow
* Reporting
* Prerequisites
* Build & Rebuild
* Quick Start
* Run Programs
* Testing
* Code Style
* C/JS Interop Hygiene
* Environment-Specific Code (files and `$if`)
* Safety
* Debug
* Compiler Architecture
* Key Directories
* Test Locations
* Error Reporting (checker/parser)
* Option/Result Types
* Tools
* Environment Variables
* Gotchas

## Agent Rules
* Be concise by default; expand only when asked for depth.
* If the user asks for in-depth detail, provide it while keeping structure tight and scan-friendly.
* Run commands from repo root.
  Assume `pwd` is repo root in this environment unless stated otherwise.
  Verify with `pwd` if a command fails due to missing paths.
  Using per-command working directories is OK; keep paths correct.
  Return to repo root after running commands in subdirectories.
* Use `./v` only to build `./vnew`; use `./vnew` for everything else.
* Read and edit all files in V repo without asking for permission.
* Only modify files required for the user request; avoid unrelated refactors.
  If duplication is already harmful, small refactors to remove it are OK when
  they are directly needed to deliver the request.
  Only refactor duplication in code you are already touching.
  "Touching" means files already modified for the request.
* Avoid unrelated file changes; call them out if present.
* Ask before large refactors or wide file touches (more than 5 files, or changes
  across multiple top-level directories like `cmd/`, `vlib/`, `examples/`).
* If you cannot complete a requested step, state the blocker and partial progress
  (what was attempted and what remains).
* Keep output easy to scan: short sections, bullets when listing, commands in backticks, no filler.
  Use a strict, operational tone.
* Ask only when required. If information is missing, ask a direct question.
* After substantial work, provide a short summary and list touched file paths.
  Substantial work means any behavioral change, or changes in more than one file.
  Always include tests run (or "Not run" with reason) in the summary.
* NEVER simplify or edit the test files to make them pass.
* For deeper edge cases, consult `CONTRIBUTING.md` and `TESTS.md`.

## Common Workflow
1. Edit the relevant files.
2. If compiler sources changed, rebuild `./vnew` with `./v -keepc -g -o ./vnew self`.
3. Format touched files and run `./vnew check-md` on touched markdown.
4. Run the smallest relevant tests for the change scope (see Testing).

See Build & Rebuild for rebuild triggers and flags.

## Reporting
* Summary must include:
  * Behavior changes (or "No behavior change").
  * Tests run (or "Not run" with reason).
  * Touched file paths.
* Docs-only changes: run `./vnew check-md file.md`; no other tests required unless
  the docs are directly exercised by tests.
* For docs-only, explicitly mention `check-md` in the summary.
* If you update `.out` files, state the rationale in the summary.
* Example summary line: `Behavior change: none` or `Behavior change: fixed X`.
* Behavior change includes output, API, error messages, and test expectations.
* If blocked, note what was attempted and why it failed.

## Prerequisites
* Toolchain: `make`, `git`, and a C compiler (`clang` or `gcc`).
* Windows: use `make.bat` for the initial build.

## Build & Rebuild
* Initial build (only if `./v` is missing): `make` (Windows: `make.bat`).
* Build `./vnew` (missing or after compiler changes):
  `./v -keepc -g -o ./vnew self`.
* This section is the source of truth for rebuild triggers.
  If a rule appears elsewhere, defer to this section.
* Rebuild triggers:
  * Compiler sources in `vlib/v/` or `cmd/v/`.
  * Core modules: `builtin`, `strings`, `os`, `strconv`, `time`.
* If `./v` exists but compiler sources changed, still rebuild `./vnew` before tests.
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

## Quick Start
* Build once (only if `./v` is missing): `make` (Windows: `make.bat`).
* Build `./vnew` (if missing or after compiler changes; see Build & Rebuild):
  `./v -keepc -g -o ./vnew self`.
* Run a file: `./vnew run file.v`.
* Run tests in a dir: `./vnew test path/to/dir/`.
* Format a file: `./vnew fmt -w file.v`.

## Code Style
* Comments: add succinct comments only when code is not self-explanatory.
  Do not delete existing comments unless they are explicitly incorrect,
  however you may correct wrong grammar or wrong spelling.
  Add V doc comments right before each new or modified public function or method.
  The V doc comments should start with the name of the fn, example: `// the_name does ...`
* Copy pasta: avoid copy pasta. If there's duplicate logic, move to a function.
* Avoid using `unsafe{ code }` blocks where possible, and minimize their scope.
* Keep Markdown lines <= 100 chars (the checker is strict).
* Avoid hidden/bidirectional Unicode characters in source/markdown files.
* Format only files you touched with `./vnew fmt -w file.v`.
* Treat new files as touched for formatting and `./vnew check-md file.md`.
* For Markdown changes, run `./vnew check-md file.md` on touched files.

## C/JS Interop Hygiene
* Avoid using `C.` or `JS.` symbols in plain `.v` files.
  * Use `.c.v` / `.js.v` files.
  * Enable `-Wimpure-v` to catch accidental impurity.
* Do not refactor existing `C.`/`JS.` uses in `.v` files unless the change
  requires it. If touching those areas, prefer moving the code to `.c.v`/`.js.v`.
* For C interop, use correct `const_` prefixes in C function redeclarations
  when needed (helps with `-cstrict` and C static analysis tooling).

## Environment-Specific Code (files and `$if`)
V supports environment-specific file suffixes. Prefer them when the whole file is
platform/backend specific.

Common patterns:
* Backend splits:
  * `*.c.v` (C backend), `*.js.v` (JS backend), `*.native.v` (native backend),
    `*.wasm.v` (WASM backend)
* Flag splits:
  * `*_d_customflag.v` is included only with `-d customflag`
  * `*_notd_customflag.v` is included only when that flag is NOT passed

Notes:
* Do not use combinatorial suffixes like `_d_flag_linux.c.v`; use `_d_flag.v`
  plus `$if linux {}` inside the file.
* For smaller platform differences, use compile-time blocks:
  * `$if windows { ... } $else { ... }`

## Safety
* Never run `./v self` without `-o ./vnew`. It overwrites the working binary.
* Always output rebuilt compiler to `./vnew` and use `./vnew` for testing.

## Run Programs
* Compile and run: `./vnew run file.v`.
* Just compile: `./vnew file.v` (creates executable).
* With debug info: `./vnew -g run file.v`.
* Debug run (C line numbers): `./vnew -keepc -cg run file.v`.
* Example: `./vnew run examples/hello_world.v`.

## Testing
Run:
* File: `./vnew path/to/file_test.v`.
* Dir: `./vnew test path/to/dir/`.
* Dir with statistics/metrics: `./vnew -stats path/to/dir/`.
* Compiler: `./vnew vlib/v/compiler_errors_test.v`.
  Note: this can take a bit over 30 seconds on M1.
* Fix outputs (only when intended): `VAUTOFIX=1 ./vnew vlib/v/compiler_errors_test.v`.
* All: `./vnew test-all`.
  Note: `./vnew test-all` can take a bit over 5 minutes on M1; run sparingly.
  Ask before running `./vnew test-all` unless explicitly requested.

When:
* Compiler changes (`vlib/v/`):
  Run `./vnew vlib/v/compiler_errors_test.v`, `./vnew test vlib/v/`.
* vlib changes: Run nearest `*_test.v` or `./vnew test vlib/path/`.
* Tool changes (`cmd/`): Run tool-specific tests. If none exist, run the
  smallest relevant `*_test.v` that exercises the tool.
  Examples: `cmd/tools/vfmt` -> `vlib/v/fmt/fmt_test.v`,
  `cmd/tools/vdoc` -> `vlib/v/doc/doc_test.v`.
* Broad refactors: Run `./vnew test-all`.
* Backend-specific changes: run the smallest relevant tests for the affected
  backend. JS/native/WASM backends are incomplete, so avoid broad
  `-b <backend> test vlib/` runs. Prefer targeted `*_test.v` files or small
  test dirs with `-b js|native|wasm`.

Types:
* Standard: `*_test.v` files with `test_` functions.
* Output: `.vv` source + `.out` expected output in `vlib/v/tests/`.

Docs-only guidance: see Reporting.

### Useful env variables and flags while testing
* `VAUTOFIX=1` - Auto-update .out files when tests fail (run twice), only when intended.
* `VTEST_ONLY=glob_pattern` - Run only tests matching pattern.
* `VTEST_HIDE_OK=1` - Hide successful tests, show only failures.
* `./vnew -progress test path/to/dir/` - Show only the currently running test.
* Output expectations: update `.out` files only when behavior changes are intended;
  note the rationale in the summary.

## Debug
* See what the C compiler is doing:
  * `-showcc` prints the C compile command.
  * `-show-c-output` prints the C compiler output.
* Keep and inspect generated C:
  * `-keepc -cg` is the common combo.
  * `-printfn main__main` prints the generated C function named `main__main` to
    stdout (C backend only). It refers to the V function `main` in the `main`
    module, using the `modulename__fnname` format. This flag can be repeated to
    print multiple functions. Methods/generics may use more complex C names;
    use `-keepc` to confirm exact symbols.
* Trace stages: `-d trace_scanner|trace_parser|trace_checker|trace_gen`.
  These flags can help diagnose a problem, when a stage stops earlier than
  expected.
* Time stages: `-d time_parsing|time_checking`.
* V panics: `-keepc -g`.
* C segfaults: `-keepc -cg -cc clang`.
* JS/native/wasm: prefer small, focused test files with `-b js|native|wasm`
  and `-show-c-output` where applicable; avoid broad `-b <backend> test vlib/`.

## Compiler Architecture
The V compiler has the following stages, orchestrated by the `v.builder` module:
`v.scanner` -> `v.parser` -> `v.checker` -> `v.transformer` -> `v.markused` -> `v.gen.c`
Their corresponding folders are: vlib/v/scanner, vlib/v/parser, vlib/v/checker,
vlib/v/transformer, vlib/v/markused, vlib/v/gen/c .
There are additional subsystems (supporting or optional compiler
modules) like v.comptime, v.generics, v.pref, v.reflection, v.callgraph, etc.

### Key Directories
* `vlib/`: Standard library.
* `cmd/tools/`: vfmt, vdoc, vup, vquest, etc.
* `examples/`: Example programs.
* `thirdparty/`: Bundled C libraries (tcc, mbedtls, sokol, etc.).
* `cmd/v/v.v`: Compiler entry.
* `vlib/v/`: Compiler modules.
  * ast/ - AST node definitions
  * fmt/ - Code formatter
  * scanner/ - Tokenizer
  * token/ - Token definitions
  * parser/ - Produces AST from tokens
  * checker/ - Type checking and resolution
  * transformer/ - Common optimisations and simplifications, makes the backends simpler
  * markused/ - Dead code eliminator
  * gen/c/ - C code generation (primary backend, known as cgen)
  * gen/js/ - JavaScript backend
  * gen/native/ - Machine code generation (ELF, Mach-O)
  * gen/wasm/ - WebAssembly backend

### Test Locations
* `vlib/v/tests/`: Compiler feature tests.
* `vlib/v/slow_tests/`: Output-matching and slow tests for the compiler.
* `vlib/v/slow_tests/inout/` - Output comparison tests (.vv + .out pairs)
* `vlib/v/parser/tests/` - Parser error tests
* `vlib/v/checker/tests/` - Checker error tests
* `vlib/v/gen/c/testdata/` - C codegen tests (.vv + .c.must_have)


## Error Reporting (checker/parser)
* Error: `c.error('message', pos)` - hard error, stops compilation.
* Warning: `c.warn('message', pos)` - warning, allows compilation.
* Notice: `c.note('message', pos)` - informational only.
* Pattern: Most checker methods use `fn (mut c Checker)` receiver.
* Location: `vlib/v/checker/errors.v`.

## Option/Result Types
* Syntax: `?Type` (optional, can be none) vs `!Type` (result, can error).
* Common bugs: Unwrapping in if guards, struct init with option fields,
  ternaries with options.
* Tests: Search `vlib/v/tests/` for option/result test files.
* Pitfall: Options in ternaries, SQL statements, and fixed arrays need
  special handling in cgen.

## Tools
* Note: if a rule overlaps with Testing, follow Testing.
* Format: `./vnew fmt -w file.v` (required before commits).
* Check *all* files are formatted: `./vnew -silent test-fmt`.
* Check markdown: `./vnew check-md file.md` (required before commits).
* Code style checker: `./vnew vet vlib/v`
* Module docs: `./vnew doc -readme -all -l module_name`.
* Search: `rg pattern` (or `git grep`); list files: `rg --files`.
* Auto-format hook: `./vnew git-fmt-hook install`.

## Environment Variables
* VFLAGS: Pass flags to all V invocations (e.g., `VFLAGS='-g' ./vnew test-all`).
* VAUTOFIX: Update test expectations, only when intended (already mentioned in Test section).
* VEXE: Path to V compiler executable (useful in CI/scripts).
* V2-specific: `V2CC`, `V2CFLAGS`, `V2VERBOSE` (for v2 development).

## Gotchas
* Core modules (`builtin`, `strings`, `os`, `strconv`, `time`) can affect the
  compiler because it is a V program. Rebuild `./vnew` when they change.
* Output tests require exact matches; whitespace changes break tests.
* Formatting reminders: follow Code Style for `fmt` and `check-md`.
* C compilation errors? Check generated C with `-keepc` (creates `/tmp/*.tmp.c`).
* Broken compiler? First rebuild `./vnew` with `./v -keepc -g -o ./vnew self`.
  Check `git status` before stashing to avoid hiding unrelated changes.
  As a last resort, run `git stash`, then `make`, then `git stash apply`
  to rebuild V from a clean slate.
* Consult `CONTRIBUTING.md` and `TESTS.md` for:
  * New tests or test infra changes.
  * Output tests or `.out` updates.
  * Broad refactors or compiler-wide changes.
