# V Repo Guide

Quick reference for the V compiler, standard library, and tools.

## Quick Start
* Build once (only if `./v` is missing): `make` (Windows: `make.bat`).
* Rebuild compiler: `./v -keepc -g -o ./vnew self`.
* Run a file: `./vnew run file.v`.
* Run tests in a dir: `./vnew test path/to/dir/`.
* Format a file: `./vnew fmt -w file.v`.

## Behavior Rules
* Always be concise.
* Run commands from repo root.
* Run commands like `./v -o vnew self`, `./v .` and `./v file.v` immediately
  without asking for permission.
* Read and edit all files in V repo wihout asking for permission.
* Keep output easy to scan. Use a strict, operational tone.
* Ask only when required. If information is missing, ask a direct question.
* After substantial work, provide a short summary and list touched file paths.
* NEVER simplify or edit the test files to make them pass.

## Code Style
* Comments: add succinct comments only when code is not self-explanatory.
  Do not delete existing comments unless they are explicitly incorrect,
  however you may correct wrong grammar or wrong spelling.
  Add V doc comments right before each public function or method.
  The V doc comments should start with the name of the fn, example: `// the_name does ...`
* Copy pasta: avoid copy pasta. If there's duplicate logic, move to a function.
* Avoid using `unsafe{ code }` blocks where possible, and minimize their scope.

## Safety
* Never run `./v self`. It overwrites the working binary.
* Always output rebuilt compiler to `./vnew` and use `./vnew` for testing.

## Build
* Initial: `make` (only when `./v` is missing; Windows: `make.bat`).
* Rebuild: `./v -keepc -g -o ./vnew self`.
* Common flags: `-g` (debug), `-prod` (optimized), `-o file` (output name),
  `-cc clang` (C compiler), `-b js|native` (backend).

## Run Programs
* Compile and run: `./vnew run file.v`.
* Just compile: `./vnew file.v` (creates executable).
* With debug info: `./vnew -g run file.v`.
* Example: `./vnew run examples/hello_world.v`.

## Testing
Run:
* File: `./vnew path/to/file_test.v`.
* Dir: `./vnew test path/to/dir/`.
* Dir with statistics/metrics: `./vnew -stats path/to/dir/`.
* Compiler: `./vnew vlib/v/compiler_errors_test.v`.
* Fix outputs: `VAUTOFIX=1 ./vnew vlib/v/compiler_errors_test.v`.
* All: `./vnew test-all`.

When:
* Compiler changes (`scanner|parser|checker|transformer|markused|gen`):
  Run `./vnew vlib/v/compiler_errors_test.v`, `./vnew test vlib/v/`.
* vlib changes: Run nearest `*_test.v` or `./vnew test vlib/path/`.
* Tool changes (`cmd/`): Run tool-specific tests.
* Broad refactors: Run `./vnew test-all`.

Types:
* Standard: `*_test.v` files with `test_` functions.
* Output: `.vv` source + `.out` expected output in `vlib/v/tests/`.

### Useful env variables and flags while testing
* `VAUTOFIX=1` - Auto-update .out files when tests fail (run twice).
* `VTEST_ONLY=glob_pattern` - Run only tests matching pattern.
* `VTEST_HIDE_OK=1` - Hide successful tests, show only failures.
* `./vnew -progress test path/to/dir/` - Show only the currently running test.

## Debug
* Trace stages: `-d trace_scanner|trace_parser|trace_checker|trace_gen`.
  These flags can help diagnose a problem, when a stage stops earlier than expected.
* Time stages: `-d time_parsing|time_checking`.
* V panics: `-keepc -g`.
* C segfaults: `-keepc -cg -cc clang`.

## Compiler Architecture
The V compiler, has the following stages, orchestrated by the `v.builder` module:
`v.scanner` -> `v.parser` -> `v.checker` -> `v.transformer` -> `v.markused` -> `v.gen.c`
Their corresponding folders are: vlib/v/scanner, vlib/v/parser, vlib/v/checker,
vlib/v/transformer, vlib/v/markused, vlib/v/gen/c .

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
* Format: `./vnew fmt -w file.v` (required before commits).
* Check *all* files are formatted: `./vnew -silent test-fmt`.
* Check markdown: `./vnew check-md file.md` (required before commits).
* Code style checker: `./vnew vet vlib/v`
* Module docs: `./vnew doc -readme -all -l module_name`.
* Search: `rg pattern` (or `git grep`); list files: `rg --files`.
* Auto-format hook: `./vnew git-fmt-hook install`.

## Environment Variables
* VFLAGS: Pass flags to all V invocations (e.g., `VFLAGS='-g' ./vnew test-all`).
* VAUTOFIX: Update test expectations (already mentioned in Test section).
* VEXE: Path to V compiler executable (useful in CI/scripts).
* V2-specific: `V2CC`, `V2CFLAGS`, `V2VERBOSE` (for v2 development).

## Gotchas
* Core modules (`builtin`, `strings`, `os`, `strconv`, `time`) affect compiler.
* Output tests require exact matches; whitespace changes break tests.
* Always format before committing: `./vnew fmt -w` and `./vnew check-md`.
* C compilation errors? Check generated C with `-keepc` (creates `/tmp/*.tmp.c`).
* Broken compiler? Run `git stash`, then `make`, then `git stash apply` to
  rebuild V from a clean slate.
