# Automated tests

TLDR: do run `v test-all` locally, after making your changes,
and before submitting PRs.

Tip: use `v -cc tcc` when compiling tests, because TCC is much faster,
compared to most other C compilers like clang/gcc/msvc. Most test commands
will use the V compiler and the V tools many times, potentially
hundreds/thousands of times.

## `v test-all`

Test and build *everything*. Useful to verify *locally*, that the CI will
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

By default, `v test` uses at most four parallel workers and budgets one worker per 8 GiB
of memory. On Linux it uses the lower of physical memory and the active cgroup memory limit.
Set `VJOBS` to a positive value to explicitly choose a different worker count when your test
workload and machine capacity are known.

Skipped test paths are resolved before comparison, so selecting a file through a symlink
does not bypass its platform or architecture exclusion.

Tests ending in `_windows_test.v` or `_windows_test.c.v` run only on Windows.
The compound `_android_outside_termux_test.v` and `_android_outside_termux_test.c.v`
suffixes select Android outside Termux; `_termux_test.v` and `_termux_test.c.v`
select Termux.

## `v test vlib/v/tests`:

This folder contains _test.v files, testing the different features of the V
compiler. Each of them will be compiled, and all the features in them have
to work (verified by assertions).

## `v vlib/v/slow_tests/inout/compiler_test.v`

This is a *test runner*, that checks whether the output of running a V program,
matches an expected .out file. You can also check for code that does panic
using this test runner - just paste the start of the `panic()` output in the
corresponding .out file.

> [!NOTE]
> These tests, expect to find a pair of `.vv` and `.out` files, in the folder:
> vlib/v/slow_tests/inout

The test runner will run each `.vv` file, and will check that its output, matches
the contents of the `.out` file with the same base name. This is particularly useful
for checking that errors and panics are printed.

## `v test vlib/v/gen/c/`

The C backend has focused unit and integration tests beside its implementation.
Many tests compile a small V source to C and assert on the generated declarations,
expressions, ABI, linker inputs, or runtime behavior.

## `v test cmd/v/`

The compiler process regressions workflow runs every launcher test in `cmd/v/` on Linux,
macOS, and Windows with the default compiler. This covers argument routing, fallback
messages, executable discovery, and tool-cache behavior, including platform-specific code.

## Line coverage

Collect coverage with `v -coverage coverage_dir path/to/file_test.v`, then inspect it with
`v cover coverage_dir`. Add `-no-skip-unused` when compiling to include uncalled functions
in the report as well as executed code.

On Windows, test statistics and coverage use a 64-bit monotonic clock resolved at runtime,
so they also work with bundled TCC versions whose import libraries omit `GetTickCount64`.

## REPL tests

The test runner for these is `vlib/v/slow_tests/repl/repl_test.v`.

The test cases for the V REPL, are stored in .repl files, in the folder
`vlib/v/slow_tests/repl/`. Each .repl file in this folder, contains several
lines of input to the V repl, followed by a single line of `===output===`,
and then the output lines, that the repl would normally show for the input
lines.

If you change the compiler or the REPL source, and you have breaks in those
.repl files, you can replace the current output in them, by running several
times `VAUTOFIX=1 ./vlib/v/slow_tests/repl/repl_test.v`, until all the
files are fixed and the test pass.

## `v vlib/v/slow_tests/run_project_folders_test.v`

This *test runner*, checks whether whole project folders, can be compiled, and run.

> [!NOTE]
> Each project in these folders, should finish with an exit code of 0,
> and it should output `OK` as its last stdout line.

## `v vlib/v/tests/known_errors/known_errors_test.v`

This *test runner*, checks whether a known program, that was expected to compile,
but did NOT, due to a buggy checker, parser or cgen, continues to fail.
The negative programs are collected in the `vlib/v/tests/known_errors/testdata/` folder.
Each of them should FAIL to compile, due to a known/confirmed compiler bug/limitation.

The intended use of this, is for providing samples, that currently do NOT compile,
but that a future compiler improvement WILL be able to compile, and to
track, whether they were not fixed incidentally, due to an unrelated
change/improvement. For example, code that triggers generating invalid C code can go here,
and later when a bug is fixed, can be moved to a proper _test.v or .vv/.out pair, outside of
the `vlib/v/tests/known_errors/testdata/` folder.

## Test building of actual V programs (examples, tools, V itself)

* `v build-tools`
* `v build-examples`
* `v build-vbinaries`

`v build-examples` discovers files with a `main`, `no_main`, or implicit main module,
and builds configured projects as folders. Leading comments, attributes, and directives do not
affect module discovery. Library modules are checked when the programs that import them compile.

## Formatting tests

`cmd/tools/vfmt_test.v` checks the formatter command, while
`vlib/v/gen/v/gen_test.v` and the other tests in `vlib/v/gen/v/` check
flat-AST-to-V formatting and round trips.

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

Note: if that command finds formatting errors, they can be fixed with:
`VAUTOFIX=1 ./v check-md file.md` or with `v check-md -fix file.md`.

## `v test-self`

Run `vlib` module tests, *including* the compiler tests.
Test discovery includes architecture-suffixed files such as `_test.amd64.v` when
the suffix matches the host architecture; files for other architectures are excluded.

To run the same suite across separate machines, set `VTEST_SELF_SHARD_COUNT` to the number of
machines and set `VTEST_SELF_SHARD_INDEX` to a different zero-based index on each one. For example,
`VTEST_SELF_SHARD_COUNT=5 VTEST_SELF_SHARD_INDEX=0 ./v test-self vlib` runs the first shard.
Every test file belongs to exactly one shard. Leave both variables unset for the full suite.

## `v vlib/v/compiler_errors_test.v`

This runs tests for:

* `vlib/v/scanner/tests/*.vv`
* `vlib/v/checker/tests/*.vv`
* `vlib/v/parser/tests/*.vv`

Some fixtures require the V 0.5.2 compatibility compiler: legacy compiler-module diagnostics,
legacy JSON code-generation errors, and the `with_check_option` cases. The current driver selects
that compiler for those fixtures; other fixtures start with the current compiler and may use its
normal compatibility fallback. The suite remains runnable on master even though the legacy
compiler sources were removed from this tree.

Prepare the compatibility compiler from the repository root before running the complete suite:

```sh
make v1
./v -silent vlib/v/compiler_errors_test.v
```

`make v1` installs the pinned V 0.5.2 release and its matching library in a cache, or builds that
release with `oldv` when a usable release binary is unavailable. Initial installation needs network
access and the installer tools; source builds also need Git and a C compiler. On Windows, run this
from MSYS2 with GNU make (`make` or `mingw32-make`) available. Building the ordinary compiler with
`makev.bat` alone does not provision this compatibility compiler.

The driver can run `make v1` automatically when needed. If it reports that no usable fallback was
found and make is unavailable, install GNU make and run the preparation command above. This is a
missing test prerequisite, and also affects the complete `v test vlib/v/` run, which includes this
suite. After installation, a usable cached compatibility compiler can run without make.

Use `-new-compiler` when investigating an individual fixture with the current compiler. It overrides
explicit legacy fixture selection, so its diagnostics need not match that fixture's existing `.out`
file. Disabling automatic fallback with `V_MACOS_V3_NO_FALLBACK=1` still permits explicit legacy
fixture selection and does not remove the suite's compatibility-compiler prerequisite.

> [!NOTE]
> There are special folders, that compiler_errors_test.v will try to
> run/compile with specific options:

vlib/v/checker/tests/globals_run/ - `-enable-globals run`;
results stored in `.run.out` files, matching the .vv ones.

NB 2: in case you need to modify many .out files, run *twice* in a row:
`VAUTOFIX=1 ./v vlib/v/compiler_errors_test.v`
This will fail the first time, but it will record the new output for each
.vv file, and store it into the corresponding .out file. The second run
should be now successful, and so you can inspect the difference, and
commit the new .out files with minimum manual effort.

NB 3: To run only some of the tests, use:
`VTEST_ONLY=mismatch ./v vlib/v/compiler_errors_test.v`
This will check only the .vv files, whose paths match the given filter.

NB 4: To run tests, but without printing status lines for all the successful
ones, use:
`VTEST_HIDE_OK=1 ./v test vlib/math/`
This will print only the total stats, and the failing tests, but otherwise
it will be silent. It is useful, when you have hundreds or thousands of
individual `_test.v` files, and you want to avoid scrolling.

NB 5: To show only *the currently running test*, use:
`./v -progress test vlib/math/`
In this mode, the output lines will be limited, no matter how many `_test.v`
files there are. The output will contain the total stats and the output of
the failing tests too.

NB 6: Set `VTEST_SKIP_OWNERSHIP=1` to omit ownership and autofree tests from
`v test`, `v test-self`, and `vlib/v/test_all.vsh`. GitHub Actions enables this
behavior automatically while ownership/autofree coverage is disabled there.

## `.github/workflows/ci.yml`

This is a Github Actions configuration file, that runs various CI
tests in the main V repository, for example:

* `v vet vlib/v` - run a style checker.
* `v test-self` (run self tests) in various compilation modes.

> [!NOTE]
The VDOC test vdoc_file_test.v now also supports VAUTOFIX, which is
useful, if you change anything inside cmd/tools/vdoc,
or inside the modules that it depends on (like markdown).
After such changes, just run this command *2 times*, and commit the
resulting changes in `cmd/tools/vdoc/testdata` as well:
`VAUTOFIX=1 ./v cmd/tools/vdoc/vdoc_file_test.v`
