# Automated tests

TLDR: do run `v test-all` locally, after making your changes,
and before submitting PRs.

Tip: use `v -cc tcc` when compiling tests, because TCC is much faster,
compared to most other C compilers like clang/gcc/msvc. Most test commands
will use the V compiler and the V tools many times, potentially 
hundreds/thousands of times.

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

# Details:
In the `v` repo there are many tests. The main types are:

## `_test.v` tests - these are the normal V test files.
All `test_` functions in these files, will be ran automatically by 
V's test framework.

NB 1: You can run test files one by one, with:
`v file_test.v` - this will run the test_ functions in file_test.v,
and will exit with a 0 exit code, if they all had 0 failing assertions.

`v -stats file_test.v` - this will run the test_ functions, and show a
report about how much time it took to run each of them too.

NB 2: You can also run many test files at once (in parallel, depending on
how many cores you have), with:
`v test folder` - this will run *all* `_test.v` files in `folder`,
recursively.

`v -stats test folder` - same, but will also produce timing reports
about how fast each test_ function in each _test.v file ran.


## `v test vlib/v/tests`:

This folder contains _test.v files, testing the different features of the V
compiler. Each of them will be compiled, and all the features in them have
to work (verified by assertions).

## `v vlib/v/tests/inout/compiler_test.v`

This is a *test runner*, that checks whether the output of running a V program,
matches an expected .out file. You can also check for code that does panic
using this test runner - just paste the start of the `panic()` output in the
corresponding .out file.

NB: these tests, expect to find a pair of `.vv` and `.out` files, in the folder:
vlib/v/tests/inout

The test runner will run each `.vv` file, and will check that its output, matches
the contents of the `.out` file with the same base name. This is particularly useful
for checking that errors and panics are printed.

## `v vlib/v/gen/c/coutput_test.v`

coutput_test.v is a *test runner*, that checks whether the generated C source
code matches *all* expectations, specified in *.c.must_have files, in the
folder vlib/v/gen/c/testdata/ .

Each `.c.must_have` file, *has* to have a corresponding `.vv` file.

Each `.c.must_have` file, consists of multiple lines. Each of these
lines, *should* be present *at least once* in the output, when the .vv
file is compiled with `-o -` .

## `v vlib/v/tests/run_project_folders_test.v`
This *test runner*, checks whether whole project folders, can be compiled, and run.

NB: Each project in these folders, should finish with an exit code of 0,
and it should output `OK` as its last stdout line.

## Test building of actual V programs (examples, tools, V itself)

* `v build-tools`
* `v build-examples`
* `v build-vbinaries`

## Formatting tests

In `vlib/v/fmt/` there are:

* `v vlib/v/fmt/fmt_test.v`

This checks `.out` tests.

* `v vlib/v/fmt/fmt_keep_test.v`

This verifies that all `_keep.vv` files in the `vlib/v/fmt/tests/` folder,
would be unchanged by `v fmt -w`, i.e. that the v source code formatter,
generates a stable source output, that does not change, once it is already
formatted once.

* `v vlib/v/fmt/fmt_vlib_test.v`

This checks that all V source files are formatted, and prints a summary.
This is not required.

* `v test-cleancode`

Check that most .v files, are invariant of `v fmt` runs.

* `v test-fmt`

This tests that all .v files in the current folder are already formatted.
It is useful for adding to CI jobs, to guarantee, that future contributions
will keep the existing source nice and clean.

## Markdown/documentation checks:

* `v check-md -hide-warnings .`

Ensure that all .md files in the project are formatted properly,
and that the V code block examples in them can be compiled/formatted too.

## `v test-self`

Run `vlib` module tests, *including* the compiler tests.

## `v vlib/v/compiler_errors_test.v`

This runs tests for:
* `vlib/v/scanner/tests/*.vv`
* `vlib/v/checker/tests/*.vv`
* `vlib/v/parser/tests/*.vv`

NB: there are special folders, that compiler_errors_test.v will try to
run/compile with specific options:

vlib/v/checker/tests/globals_run/ - `-enable-globals run`; 
results stored in `.run.out` files, matching the .vv ones.

NB 2: in case you need to modify many .out files, run *twice* in a row:
`VAUTOFIX=1 ./v vlib/v/compiler_errors_test.v` 
This will fail the first time, but it will record the new output for each
.vv file, and store it into the corresponding .out file. The second run
should be now successfull, and so you can inspect the difference, and 
commit the new .out files with minimum manual effort.

NB 3: To run only some of the tests, use:
`VTEST_ONLY=mismatch ./v vlib/v/compiler_errors_test.v`
This will check only the .vv files, whose paths match the given filter.

## `.github/workflows/ci.yml`

This is a Github Actions configuration file, that runs various CI
tests in the main V repository, for example:

* `v vet vlib/v` - run a style checker.
* `v test-self` (run self tests) in various compilation modes.
