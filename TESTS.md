# Automated tests

In the `v` repo there are several different tests. The main types are:

* `_test.v` tests - check that `test_` functions succeed. These can be 
run per directory or individually.
* `.out` tests - run a `.vv` file and check the output matches the 
contents of the `.out` file with the same base name. This is 
particularly useful for checking that errors are printed.

Tip: use `v -cc tcc` when compiling tests for speed.


## `v test-compiler`

This builds and tests:

* `vc` repo
* `_test.v` files
* examples
* `v install` a module

Some of these can be invoked separately.

## `vlib/v/compiler_errors_test.v`

This runs tests for:
* `checker/tests/*.vv`
* `parser/tests/*.vv`

## `vlib/v/tests`

General runnable program tests.

## Actual code

* `v build-examples`
* `v build-tools`

## `v test-fixed`

Test `vlib`.

## vfmt tests

In `vlib/v/fmt/` there's:

* `fmt_test.v`

This checks `.out` tests.

* `fmt_keep_test.v`

This verifies that `_keep.v` files would be unchanged by `vfmt -w`.

* `fmt_vlib_test.v`

This checks all source files are formatted and prints a summary. This 
is not required.

* `v test-fmt`

Test all files in the current directory are formatted.

## `.github/workflows/ci.yml`

This runs various CI tests, e.g.:

* `v vet vlib/v` - style checker
* `v fmt -verify` on certain source files
