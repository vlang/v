# V Repo Guide

Quick reference for V compiler, standard library, and tools. Run commands from repo root.

## Build
* **Initial**: `make` (only when `./v` missing; Windows: `make.bat`)
* **Rebuild**: `./v -keepc -g -o ./vnew self` (use `./vnew` for testing)
* **Critical**: Never `./v self` - overwrites working binary. Always output to `vnew`.
* **Common flags**: `-g` (debug), `-prod` (optimized), `-o file` (output name),
  `-cc clang` (C compiler), `-b js|native` (backend)

## Run Programs (test your changes)
* **Compile and run**: `./vnew run file.v`
* **Just compile**: `./vnew file.v` (creates executable)
* **With debug info**: `./vnew -g run file.v`
* **Examples**: `./vnew run examples/hello_world.v`

## Test
**Run:**
* All: `./vnew test-all`
* File: `./vnew path/to/file_test.v`
* Dir: `./vnew test path/to/dir/` (add `-stats` for metrics)
* Compiler: `./vnew vlib/v/compiler_errors_test.v`
* Fix outputs: `VAUTOFIX=1 ./vnew vlib/v/compiler_errors_test.v` (updates `.out` files)

**When:**
* Compiler changes (`scanner|parser|checker|transformer|markused|gen`):
  Run `./v compiler_errors_test.v` + `./v test ` and the dir with the change.
* vlib changes: Run nearest `*_test.v` or `./vnew test vlib/path/`
* Tool changes (`cmd/`): Run tool-specific tests
* Broad refactors: Run `test-all`

**Types:**
* Standard: `*_test.v` files with `test_` functions
* Output: `.vv` source + `.out` expected output in `vlib/v/tests/`

## Debug
* **Trace stages**: `-d trace_scanner|trace_parser|trace_checker|trace_gen`
* **Time stages**: `-d time_parsing|time_checking`
* **V panics**: `-keepc -g`
* **C segfaults**: `-keepc -cg -cc clang`

## Structure
* `cmd/v/v.v`: Compiler entry
* `vlib/v/`: Compiler pipeline
  * `scanner/` → `parser/` → `checker/` → `transformer` → `markused` →
    `gen/c|js|native/` → `builder/`
* `vlib/v/tests/`: — Compiler feature tests
* `vlib/v/slow_tests/`: — Output-matching and slow tests for the compiler
* `vlib/`: Standard library
* `cmd/tools/`: vfmt, vdoc, vup, vquest, etc.
* `examples/`: Example programs
* `thirdparty/`: Bundled C libraries (tcc, mbedtls, sokol, etc.)

## Error Reporting (checker/parser)
* **Error**: `c.error('message', pos)` - Hard error, stops compilation
* **Warning**: `c.warn('message', pos)` - Warning, allows compilation
* **Notice**: `c.note('message', pos)` - Informational only
* **Pattern**: Most checker methods use `fn (mut c Checker)` receiver
* **Location**: See `vlib/v/checker/errors.v` for API details

## Option/Result Types
* **Syntax**: `?Type` (optional, can be none) vs `!Type` (result, can error)
* **Common bugs**: Unwrapping in if guards, struct init with option fields, ternaries with options
* **Tests**: Search `vlib/v/tests/` for option/result test files - check for similar patterns
* **Pitfall**: Options in ternaries, SQL statements, and fixed arrays need special handling in cgen

## Tools
* **Format**: `./vnew fmt -w file.v` (required before commits)
* **Check markdown**: `./vnew check-md file.md`
* **Module docs**: `./vnew doc -readme -all -l module_name`
* **Search**: `rg pattern` (or `git grep`); list files: `rg --files`
* **Auto-format hook**: `./vnew git-fmt-hook install`

## Environment Variables
* **VFLAGS**: Pass flags to all V invocations (e.g., `VFLAGS='-g' ./v test-all`)
* **VAUTOFIX**: Update test expectations (already mentioned in Test section)
* **VEXE**: Path to V compiler executable (useful in CI/scripts)
* **V2-specific**: `V2CC`, `V2CFLAGS`, `V2VERBOSE` (for v2 development)

## Gotchas
* Core modules (`builtin`, `strings`, `os`, `strconv`, `time`) affect compiler - be careful
* Output tests require exact matches - whitespace changes break tests
* Always format before committing: `./vnew fmt -w`
* C compilation errors? Check generated C with `-keepc` (creates `/tmp/*.tmp.c` files)
* Broken compiler? Run `make` to rebuild from C bootstrap
