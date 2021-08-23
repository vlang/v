# Automated tests

TLDR: run `v test-all` locally, after making your changes,
and before submitting PRs.

## Notes
In the `v` repo there are several different tests. The main types are:

* `_test.v` tests - check that `test_` functions succeed. These can be
run per directory or individually.
* `.out` tests - run a `.vv` file and check the output matches the
contents of the `.out` file with the same base name. This is
particularly useful for checking that errors are printed.

Tip: use `v -cc tcc` when compiling tests for speed.

## `vlib/v/tests`

General runnable tests for different features of the V compiler.

* `vlib/v/tests/inout/compiler_test.v`

Test output of running a V program matches an expected .out file.
Check the source for how to test panics.

* `vlib/v/gen/c/coutput_test.v`

This tests whether the generated C source code matches all expectations,
specified in *.c.must_have files, in the folder vlib/v/gen/c/testdata/ .

Each `.c.must_have` file has to have a corresponding .vv file.

Each `.c.must_have` file, consists of multiple lines. Each of these
lines, *should* be present *at least once* in the output, when the .vv
file is compiled with `-o -` .

* `vlib/v/tests/run_project_folders_test.v`
Tests whether whole project folders can be compiled, and run.
NB: Each project in these folders, should finish with exit code 0,
and it should output `OK` as its last stdout line.

## Test building of actual V programs (examples, tools, V itself)

* `v build-tools`
* `v build-examples`
* `v build-vbinaries`

## vfmt tests

In `vlib/v/fmt/` there are::

* `v vlib/v/fmt/fmt_test.v`

This checks `.out` tests.

* `v vlib/v/fmt/fmt_keep_test.v`

This verifies that `_keep.v` files would be unchanged by `vfmt -w`.

* `v vlib/v/fmt/fmt_vlib_test.v`

This checks all source files are formatted and prints a summary.
This is not required.

* `v test-fmt`

Test all files in the current directory are formatted.

## Markdown

* `v check-md -hide-warnings .`

Ensure that all .md files in the project are formatted properly,
and that the V code block examples in them can be compiled/formatted too.

## `.github/workflows/ci.yml`

This runs various CI tests, e.g.:

* `v vet vlib/v` - style checker
* `v fmt -verify` on certain source files

## `v test-cleancode`

Check that most .v files, are invariant of `v fmt` runs.

## `v test-self`

Run `vlib` module tests, *including* the compiler tests.

## `v vlib/v/compiler_errors_test.v`

This runs tests for:
* `vlib/v/checker/tests/*.vv`
* `vlib/v/parser/tests/*.vv`

### Special folders that compiler_errors_test.v will try to
run/compile with specific options:

vlib/v/checker/tests/globals_run/ - `-enable-globals run`; 
results stored in `.run.out` files, matching the .vv ones.

## `v test-all`

Test and build *everything*. Usefull to verify *locally*, that the CI will
most likely pass. Slowest, but most comprehensive.

It works, by running these in succession:
* `v test-cleancode`
* `v test-self`
* `v test-fmt`
* `v build-tools`
* `v build-examples`
* `v check-md -hide-warnings .`
* `v install nedpals.args`
